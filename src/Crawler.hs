{-# LANGUAGE OverloadedStrings, MultiWayIf, BangPatterns #-}

module Main where

import Types
import IssueParser
import ProjectParser

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment
import System.IO

output :: Output -> IssueMeta -> IO ()
output PrintOutput info = do
  BS.putStrLn $ BS.concat ["** Issue ", issueOwner info, "/", issueProject info, "/", BS.pack $ show $ issueNumber info]
  print info
output (SolrOutput address) info = undefined

combineInfo :: IssueAddress -> Project -> Issue -> IssueMeta
combineInfo addr project issue = IssueMeta (genId addr)
                                 (addressOwner addr)
                                 (addressProject addr)
                                 (addressIssueId addr)
                                 (issTitle issue)
                                 (issBody issue)
                                 (S.fromAscList $ map fst $ M.toAscList $ projectLanguages project)
                                 (projectFrameworks project)
                                 (projectSize project)
                                 (projectStars project)
                                 (projectWatches project)
                                 (issDiscussion issue)
                                 (issQuality issue)
                                 (issDue issue)
                                 (issDiscusses issue)
                                 (issTags issue)

parseProject :: Output -> BS.ByteString -> BS.ByteString -> IO ()
parseProject out owner proj = do
  BS.putStrLn $ BS.concat ["* Ok, crawler, parse `", owner, "/", proj, "'"]
  eproject <- projectInfo owner proj
  case eproject of
    Left err -> return ()
    Right project -> do
      let issues = projectIssues project
      forM_ issues $ \id -> do
        eissue <- issueInfo owner proj id
        case eissue of
          Left err' -> return ()
          Right issue -> output out $ combineInfo (IssueAddress owner proj id) project issue

main = do
  args <- getArgs
  let !out = if length args == 2 && args!!1 == "Print"
             then PrintOutput
             else if length args == 3 && args!!1 == "Solr"
                  then SolrOutput $ BS.pack $ args!!2
                  else error "Usage: issue-recommendation-crawler <file-with-projects> [ Print | Solr <address> ]"
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