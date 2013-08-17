{-# LANGUAGE OverloadedStrings, MultiWayIf, BangPatterns #-}

module Main where

import Types

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import System.Environment
import System.IO

parseProject :: Output -> BS.ByteString -> BS.ByteString -> IO ()
parseProject out owner repo = do
  BS.putStrLn $ BS.concat ["Ok, crawler, parse `", owner, "/", repo, "'"]

main = do
  args <- getArgs
  let !out = if | length args == 2 && args!!1 == "Print" -> PrintOutput
                | length args == 3 && args!!1 == "Solr" -> SolrOutput $ BS.pack $ args!!2
                | True -> error "Usage: issue-recommendation-crawler <file-with-projects> [ Print | Solr <address> ]"
      file = args!!0
  withFile file ReadMode $ \f -> do
    content <- BS.hGetContents f
    let lines = BS.lines content
    forM_ lines $ \line -> let
      words = BS.words line
      owner = words!!0
      project = words!!1
      in if BS.head line == '#' 
         then return () 
         else parseProject out owner project