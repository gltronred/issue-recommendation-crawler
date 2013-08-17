{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Types where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Aeson.TH
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock (UTCTime (..))

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

data IssueMeta = IssueMeta { issueAddress :: IssueAddress
                           , issueLanguages :: M.Map BS.ByteString Double
                           , issueFrameworks :: S.Set BS.ByteString
                           , issueSize :: Int
                           , issueStars :: Int
                           , issueWatches :: Int
                           , issueDiscussion :: Int
                           , issueQuality :: Double
                           , issueDue :: UTCTime
                           , issueDiscusses :: S.Set BS.ByteString
                           } deriving (Eq,Show)

$(deriveJSON (drop 5) ''IssueMeta)

