{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

module ProjectParser where

import Types
import Network

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Text as T
import GHC.Generics

data ProjectResp = PR { size :: Int
                      , watchers :: Int } deriving (Eq,Show,Generic)
instance FromJSON ProjectResp

projectInfo :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString Project)
projectInfo owner' proj' = let
  owner = BS.unpack owner'
  proj = BS.unpack proj'
  in do
  mpr <- generalNetwork ("repos/" ++ owner ++ "/" ++ proj) Nothing Nothing (return . Just)
  case mpr of
    Nothing -> return $ Left "Some error occured"
    Just pr -> do
      eis <- projectIssueList owner' proj'
      is <- case eis of
        Left err -> do
          BS.putStrLn $ BS.concat ["Some error occured: ", err]
          return []
        Right i -> return i
      return $ Right $ Project is M.empty S.empty (size pr) (-1) (watchers pr)

data Label = Label { name :: BS.ByteString } deriving (Eq,Show,Generic,Ord)
instance FromJSON Label

data IssueResp = IR { number :: Int
                    , title :: T.Text
                    , body :: T.Text
                    , comments :: Int
                    , labels :: S.Set Label } deriving (Eq,Show,Generic)
instance FromJSON IssueResp

projectIssueList :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString [Issue])
projectIssueList owner' proj' = let
  owner = BS.unpack owner'
  proj = BS.unpack proj'
  future = UTCTime (fromGregorian 2030 1 1) (secondsToDiffTime 0)
  in do
    mis <- generalNetwork ("repos/" ++ owner ++ "/" ++ proj ++ "/issues") (Just [("state","open"), ("assignee", "none")]) Nothing (return . Just)
    case mis of
      Nothing -> return $ Left "Some error occured"
      Just is -> return $ Right $ map (\(IR n t b c ls) -> Issue n t b c 0.0 future (-1) $ S.map name ls) is

