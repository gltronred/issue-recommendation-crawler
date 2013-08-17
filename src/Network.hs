{-# LANGUAGE OverloadedStrings, ImplicitParams #-}

module Network where

import Control.Monad (when)
import Data.Aeson
import Data.Bits
import Data.List
import Data.Maybe
import Network.HTTP.Conduit
import Network.HTTP.Types (statusCode)

solrUrl = "http://cll.niimm.ksu.ru:8080/solr/"
url = "https://api.github.com/"
secretToken = "5826a377860cc45943554452a26b9b798aa6cc8b"
authParam = "?access_token=" ++ secretToken

generalNetwork :: (FromJSON r, ?verbose :: Int) => String -> Maybe [(String, String)] -> a -> (r -> IO a) -> IO a
generalNetwork path params rErr parseResponse = do
  let paramStr' = concat $ intersperse "&" $ map (\(a,b) -> a ++ "=" ++ b) $ fromJust params
      paramStr = if isNothing params || (null $ fromJust params) then "" else '&':paramStr'
  req' <- parseUrl $ url ++ path ++ authParam ++ paramStr
  let req = req' { method = "GET", requestHeaders = [("User-Agent", "Issue Recommendation Crawler")], checkStatus = \_ _ _ -> Nothing, responseTimeout = Just 20000000 }
  when (?verbose .&. 4 > 0) $ print req
  result <- withManager $ httpLbs req
  when (?verbose .&. 4 > 0) $ print result
  case statusCode $ responseStatus result of
    200 -> parseResponse $ fromJust $ decode $ responseBody result
    _ -> return rErr

sendSolr :: (ToJSON a) => a -> IO ()
sendSolr x = do
  req' <- parseUrl $ solrUrl ++ "update/json?commit=true"
  let req = req' { responseTimeout = Just 20000000, requestBody = RequestBodyLBS $ encode x }
  result <- withManager $ httpLbs req
  return ()

