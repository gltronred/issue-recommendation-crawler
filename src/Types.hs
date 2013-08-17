{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Types where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import Data.Char
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock (UTCTime (..))
import qualified Github.Issues as G

auth = G.GithubOAuth "5826a377860cc45943554452a26b9b798aa6cc8b"

data Output = PrintOutput
            | SolrOutput { solrAddress :: BS.ByteString } deriving (Eq,Show)

data IssueAddress = IssueAddress { addressOwner :: BS.ByteString
                                 , addressProject :: BS.ByteString
                                 , addressIssueId :: Int
                                 } deriving (Eq,Show)

instance FromJSON IssueAddress where
  parseJSON (Object v) = IssueAddress <$>
                         v .: "owner" <*>
                         v .: "project" <*>
                         v .: "id"
  parseJSON _ = mzero

instance ToJSON IssueAddress where
  toJSON (IssueAddress o p i) = object [ "owner" .= o
                                       , "project" .= p
                                       , "id" .= i ]

genId :: IssueAddress -> BS.ByteString
genId (IssueAddress o p i) = BS.concat [o,"/",p,"/",BS.pack $ show i]

data IssueMeta = IssueMeta { issueId :: BS.ByteString
                           , issueOwner :: BS.ByteString
                           , issueProject :: BS.ByteString
                           , issueNumber :: Int
                           , issueLanguages :: S.Set BS.ByteString
                           , issueFrameworks :: S.Set BS.ByteString
                           , issueSize :: Int
                           , issueStars :: Int
                           , issueWatches :: Int
                           , issueDiscussion :: Int
                           , issueQuality :: Double
                           , issueDue :: UTCTime
                           , issueDiscusses :: Int
                           , issueTags :: S.Set BS.ByteString
                           } deriving (Eq,Show)

$(deriveJSON (map toLower . drop 5) ''IssueMeta)


data Project = Project { projectIssues :: [Int]
                       , projectLanguages :: M.Map BS.ByteString Double
                       , projectFrameworks :: S.Set BS.ByteString
                       , projectSize :: Int
                       , projectStars :: Int
                       , projectWatches :: Int
                       } deriving (Eq,Show)

data Issue = Issue { issDiscussion :: Int
                   , issQuality :: Double
                   , issDue :: UTCTime
                   , issDiscusses :: Int
                   , issTags :: S.Set BS.ByteString
                   } deriving (Eq,Show)

data User = User { userLanguages :: S.Set BS.ByteString
                 , userFrameworks :: S.Set BS.ByteString
                 } deriving (Eq,Show)

$(deriveJSON (map toLower . drop 4) ''User)


