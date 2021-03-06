{-# LANGUAGE OverloadedStrings, BangPatterns, ImplicitParams #-}

module Main where

import Types
import Network
import IssueParser
import ProjectParser

import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import System.Environment
import System.IO

output :: (?verbose :: Int) => Output -> [IssueMeta] -> IO ()
output PrintOutput infos = do
  forM_ infos $ \info -> do
    BS.putStrLn $ BS.concat ["** Issue ", issueOwner info, "/", issueProject info, "/", BS.pack $ show $ issueNumber info]
    print info
output SolrOutput infos = do
  forM_ infos $ \info -> BS.putStrLn $ BS.concat ["** Issue ", issueOwner info, "/", issueProject info, "/", BS.pack $ show $ issueNumber info]
  sendSolr infos

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
                                 (if isJust $ issDue issue then fromJust $ issDue issue else future)
                                 (issDiscusses issue)
                                 (issTags issue)

parseProject :: (?verbose :: Int) => Output -> BS.ByteString -> BS.ByteString -> IO ()
parseProject out owner proj = do
  BS.putStrLn $ BS.concat ["* Ok, crawler, parse `", owner, "/", proj, "'"]
  eproject <- projectInfo owner proj
  case eproject of
    Left err -> BS.putStrLn $ BS.concat ["Error: ", err]
    Right project -> do
      let issues = projectIssues project
      output out $ map (\i -> combineInfo (IssueAddress owner proj $ issNumber i) project i) issues

main = do
  args <- getArgs
  let !out = if length args >= 2 && args!!1 == "Print"
             then PrintOutput
             else if length args >= 2 && args!!1 == "Solr"
                  then SolrOutput
                  else error "Usage: issue-recommendation-crawler <file-with-projects> [ Print | Solr ] [ verboseness ]"
      verbose = if length args > 2
                then read $ args!!2
                else 0
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
         else let ?verbose = verbose in parseProject out owner project