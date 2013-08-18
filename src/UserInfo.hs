{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

module UserInfo (userRepos, userInfo) where

import Types
import Network
import ProjectParser
import FrameworkParser

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import GHC.Generics

data UserResp = UR { name :: BS.ByteString } deriving (Eq,Show,Generic)
instance FromJSON UserResp

userRepos :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> IO (Maybe [BS.ByteString])
userRepos owner' token = let
  owner = BS.unpack owner'
  in do
  generalNetwork ("users/" ++ owner ++ "/repos") [("access_token",BS.unpack token)] Nothing (return . Just . map name)

userInfo :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> IO (Maybe User)
userInfo owner token = do
  mrs <- userRepos owner token
  case mrs of
    Nothing -> return Nothing
    Just rs -> do
      res <- forM rs $ \r -> do
        l <- projectLangs owner r
        f <- frameworksFor owner r
        return (maybe S.empty M.keysSet l,f)
      let (ls,fs) = unzip res
      return $ Just $ User (S.unions ls) (S.unions fs)


