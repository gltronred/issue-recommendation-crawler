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
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..),secondsToDiffTime)
import qualified Data.Text as T
import Text.Regex.PCRE.Light (Regex (..))

data Output = PrintOutput
            | SolrOutput deriving (Eq,Show)

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

future = UTCTime (fromGregorian 2030 1 1) (secondsToDiffTime 0)

genId :: IssueAddress -> BS.ByteString
genId (IssueAddress o p i) = BS.concat [o,"/",p,"/",BS.pack $ show i]

data IssueMeta = IssueMeta { issueId :: BS.ByteString
                           , issueOwner :: BS.ByteString
                           , issueProject :: BS.ByteString
                           , issueNumber :: Int
                           , issueTitle :: T.Text
                           , issueBody :: T.Text
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


data Project = Project { projectIssues :: [Issue]
                       , projectLanguages :: M.Map BS.ByteString Double
                       , projectFrameworks :: S.Set BS.ByteString
                       , projectSize :: Int
                       , projectStars :: Int
                       , projectWatches :: Int
                       } deriving (Eq,Show)

data Issue = Issue { issNumber :: Int
                   , issTitle :: T.Text
                   , issBody :: T.Text
                   , issDiscussion :: Int
                   , issQuality :: Double
                   , issDue :: Maybe UTCTime
                   , issDiscusses :: Int
                   , issTags :: S.Set BS.ByteString
                   } deriving (Eq,Show)

data User = User { userLanguages :: S.Set BS.ByteString
                 , userFrameworks :: S.Set BS.ByteString
                 } deriving (Eq,Show)

$(deriveJSON (map toLower . drop 4) ''User)


data UserIn = UserIn { userInName :: BS.ByteString
                     , userInToken :: BS.ByteString
                     } deriving (Eq,Show)

$(deriveJSON (map toLower . drop 6) ''UserIn)


data Framework = Framework { frameworkName :: BS.ByteString
                           , frameworkMask :: String
                           , frameworkRegexp :: Regex } deriving (Eq,Show)