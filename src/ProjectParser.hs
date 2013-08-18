{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

module ProjectParser (projectInfo, projectIssueList, projectLangs) where

import Types
import Network
import FrameworkParser

import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Text as T
import GHC.Generics

data ProjectResp = PR { size :: Int
                      , watchers :: Int
                      , open_issues :: Int } deriving (Eq,Show,Generic)
instance FromJSON ProjectResp

projectInfo :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> IO (Either BS.ByteString Project)
projectInfo owner' proj' = let
  owner = BS.unpack owner'
  proj = BS.unpack proj'
  in do
  mpr <- generalNetwork ("repos/" ++ owner ++ "/" ++ proj) [] Nothing (return . Just)
  case mpr of
    Nothing -> return $ Left "Some error occured"
    Just pr -> do
      eis <- projectIssueList owner' proj' (Just 1) $ open_issues pr`div`20
      is <- case eis of
        Left err -> do
          BS.putStrLn err
          return []
        Right i -> return i
      els <- projectLangs owner' proj'
      ls <- case els of
        Nothing -> do
          putStrLn "Some error while getting langs"
          return M.empty
        Just l -> return l
      fs <- frameworksFor owner' proj'
      return $ Right $ Project is ls fs (size pr) (-1) (watchers pr)

data Label = Label { name :: BS.ByteString } deriving (Eq,Show,Generic,Ord)
instance FromJSON Label

data Milestone = Milestone { due_on :: UTCTime } deriving (Eq,Show,Generic)
instance FromJSON Milestone

data IssueResp = IR { number :: Int
                    , title :: T.Text
                    , body :: T.Text
                    , comments :: Int
                    , milestone :: Maybe Milestone
                    , labels :: S.Set Label } deriving (Eq,Show,Generic)
instance FromJSON IssueResp

projectIssueList :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> Maybe Int -> Int -> IO (Either BS.ByteString [Issue])
projectIssueList owner' proj' mpage total = let
  owner = BS.unpack owner'
  proj = BS.unpack proj'
  in do
    mis <- generalNetwork ("repos/" ++ owner ++ "/" ++ proj ++ "/issues") [("page", if isJust mpage then show $ fromJust mpage else "1"), ("state","open"), ("assignee", "none")] Nothing (return . Just)
    case mis of
      Nothing -> return $ Left "Some error occured"
      Just is -> let
        getDue mm = if isJust mm then Just $ due_on $ fromJust mm else Nothing
        in do
          -- -- get rest of the list
          -- Right next <- case mpage of
          --   Just pg -> if pg<total
          --              then projectIssueList owner' proj' (Just $ pg+1) total
          --              else return $ Right []
          --   Nothing -> if 2<total
          --              then projectIssueList owner' proj' (Just 2) total
          --              else return $ Right []
          return $ Right $ (map (\(IR n t b c mm ls) -> Issue n t b c 0.0 (getDue mm) (-1) $ S.map name ls) is)

projectLangs :: (?verbose :: Int) => BS.ByteString -> BS.ByteString -> IO (Maybe (M.Map BS.ByteString Double))
projectLangs owner' proj' = let
  owner = BS.unpack owner'
  proj = BS.unpack proj'
  in do
    generalNetwork ("repos/" ++ owner ++ "/" ++ proj ++ "/languages") [] Nothing (return . Just)

