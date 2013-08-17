{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module IssueParser where

import Types
import Network

import qualified Data.ByteString as BS

issueInfo :: BS.ByteString -> BS.ByteString -> Int -> IO (Either String Issue)
issueInfo owner' project' id = let
  owner = BS.unpack owner'
  project = BS.unpack project'
  in do
  return $ Left ""

