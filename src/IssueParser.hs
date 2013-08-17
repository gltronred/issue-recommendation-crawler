{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module IssueParser where

import Types

import qualified Data.ByteString as BS
import qualified Github.Issues as G

issueInfo :: BS.ByteString -> BS.ByteString -> Int -> IO (Either String Issue)
issueInfo owner project id = do
  return $ Left ""

