{-# LANGUAGE OverloadedStrings, TemplateHaskell, DeriveGeneric, ImplicitParams #-}

module FrameworkParser (frameworksFor) where

import Types
import Network

import Control.Monad
import qualified Data.Aeson as A
import Data.Aeson.TH
import Data.Array ((!))
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Set as S
import GHC.Generics
import Text.Regex.Posix
import Text.Regex.Posix.ByteString

data Tree = Tree { path :: BS.ByteString
                 , sha :: BS.ByteString } deriving (Eq,Show,Generic)
instance A.FromJSON Tree
instance A.ToJSON Tree

mkRegex :: BS.ByteString -> Regex
mkRegex str = makeRegex str

packageMgrs :: [Framework]
packageMgrs = [Framework (mkRegex "^Gemfile$") (mkRegex "gem '([^']*)'")
              ,Framework (mkRegex ".*\\.cabal") (mkRegex "\\bbuild-depends\\b[\\:].*[\\w]+,(?=[^,]+$)")
--              ,Framework (mkRegex "pom\\.xml") (mkRegex "<dependencies\\b[^>]*>(.*?)</dependencies>")
              ]

hasFile :: Regex -> [Tree] -> Maybe BS.ByteString
hasFile mask [] = Nothing
hasFile mask ((Tree path sha):files) = if matchTest mask path
                                       then Just path
                                       else hasFile mask files

data ContentResp = CR { content :: BS.ByteString } deriving (Eq,Show,Generic)
instance A.FromJSON ContentResp

getFrameworks :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> BS.ByteString -> Regex -> IO [BS.ByteString]
getFrameworks owner' proj' path' regexp = let
  owner = BS.unpack owner'
  proj = BS.unpack proj'
  path = BS.unpack path'
  in do
    mcont <- generalNetwork' ("repos/" ++ owner ++ "/" ++ proj ++ "/contents/" ++ path) [] [("Accept","application/vnd.github.v3.raw")] Nothing (return . Just)
    case mcont of
      Nothing -> return []
      Just content -> return $ getFrameworks' (LBS.toStrict content) regexp

getFrameworks' :: BS.ByteString -> Regex -> [BS.ByteString]
getFrameworks' content regexp = let
  getMatch m = BS.take length $ BS.drop offset content
    where length = snd $ m!1
          offset = fst $ m!1
  ms = matchAll regexp content
  in if null ms
     then []
     else map getMatch ms

data Object = Object { objType :: BS.ByteString
                     , objSHA :: BS.ByteString
                     , objUrl :: BS.ByteString } deriving (Eq,Show)
$(deriveJSON (map toLower . drop 3) ''Object)

data Commit = Commit { ref :: BS.ByteString
                     , url :: BS.ByteString
                     , object :: Object } deriving (Eq,Show,Generic)
instance A.FromJSON Commit

getRef :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> IO (Maybe BS.ByteString)
getRef owner' proj' = let
  owner = BS.unpack owner'
  proj = BS.unpack proj'
  in do
    generalNetwork ("repos/" ++ owner ++ "/" ++ proj ++ "/git/refs/heads/master") [] Nothing (return . Just . (objSHA . object))

data TreeResp = TR { trSHA :: BS.ByteString
                   , trUrl :: BS.ByteString
                   , trTree :: [Tree] } deriving (Eq,Show)
$(deriveJSON (map toLower . drop 2) ''TreeResp)

getTree :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> BS.ByteString -> IO (Maybe [Tree])
getTree owner' proj' sha' = let
  owner = BS.unpack owner'
  proj = BS.unpack proj'
  sha = BS.unpack sha'
  in do
    generalNetwork ("repos/" ++ owner ++ "/" ++ proj ++ "/git/trees/" ++ sha) [] Nothing (return . Just . trTree)

frameworksFor :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> IO (S.Set BS.ByteString)
frameworksFor owner proj = do
  mLastRef <- getRef owner proj
  case mLastRef of
    Nothing -> return S.empty
    Just lastRef -> do
      mfiles <- getTree owner proj lastRef
      case mfiles of
        Nothing -> return S.empty
        Just files -> do
          (found, res) <- foldM (tryPackageMgr owner proj files) (False,[]) packageMgrs
          return $ S.fromList res

tryPackageMgr :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> [Tree] -> (Bool,[BS.ByteString]) -> Framework -> IO (Bool,[BS.ByteString])
tryPackageMgr _ _ _ b@(True,fs) _ = return b
tryPackageMgr owner proj files (False,[]) (Framework mask regexp) = let
  mfile = hasFile mask files
  in case mfile of
    Nothing -> return (False,[])
    Just file -> do
      fs <- getFrameworks owner proj file regexp
      return (True, fs)


