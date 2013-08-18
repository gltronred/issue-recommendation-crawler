{-# LANGUAGE OverloadedStrings, ImplicitParams #-}

module Network where

import Control.Monad (when)
import Data.Aeson
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Network.HTTP.Conduit
import Network.HTTP.Types (statusCode,Header)

solrUrl = "http://cll.niimm.ksu.ru:8080/solr/"
url = "https://api.github.com/"
secretToken = "5826a377860cc45943554452a26b9b798aa6cc8b"

generalNetwork :: (FromJSON r, ?verbose :: Int) => String -> [(String, String)] -> a -> (r -> IO a) -> IO a
generalNetwork path params rErr parseResponse = generalNetwork' path params [] rErr (parseResponse . fromJust . decode)

generalNetwork' :: (?verbose :: Int) => String -> [(String, String)] -> [Header] -> a -> (LBS.ByteString -> IO a) -> IO a
generalNetwork' path params headers rErr parseResponse = do
  let paramStr' = concat $ intersperse "&" $ map (\(a,b) -> a ++ "=" ++ b) $ M.toList $ M.fromList $ ("access_token","5826a377860cc45943554452a26b9b798aa6cc8b"):params
      paramStr = '?':paramStr'
  req' <- parseUrl $ url ++ path ++ paramStr
  let req = req' { method = "GET", requestHeaders = ("User-Agent", "Issue Recommendation Crawler"):headers, checkStatus = \_ _ _ -> Nothing, responseTimeout = Just 20000000 }
  when (?verbose .&. 4 > 0) $ print req
  result <- withManager $ httpLbs req
  when (?verbose .&. 4 > 0) $ print result
  case statusCode $ responseStatus result of
    200 -> parseResponse $ responseBody result
    _ -> return rErr

sendSolr :: (ToJSON a, ?verbose :: Int) => a -> IO ()
sendSolr x = do
  req' <- parseUrl $ solrUrl ++ "update/json?commit=true"
  let req = req' { method = "POST", responseTimeout = Just 20000000, requestBody = RequestBodyLBS $ encode x }
  when (?verbose .&. 8 > 0) $ print req
  result <- withManager $ httpLbs req
  when (?verbose .&. 8 > 0) $ print result
  return ()

