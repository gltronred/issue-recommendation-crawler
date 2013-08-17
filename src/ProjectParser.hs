{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module ProjectParser where

import Types

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Github.Data.Definitions
import Github.Repos
import qualified Github.Issues as G

projectInfo :: BS.ByteString -> BS.ByteString -> IO (Either String Project)
projectInfo owner' proj' = let
  owner = BS.unpack owner'
  proj = BS.unpack proj'
  in do
  elang <- languagesFor owner proj
  langs <- case elang of
    Left err -> do
      putStrLn $ "Error while getting languages: " ++ show err
      return M.empty
    Right lang -> return $ M.fromList $ map (\(Language l i) -> (BS.pack l,fromIntegral i)) $ lang
  eissues <- G.issuesForRepo' (Just auth) owner proj [G.AnyMilestone, G.Open, G.Unassigned]
  issues <- case eissues of
    Left err -> do
      putStrLn $ "Error while getting issues: " ++ show err
      return []
    Right is -> return $ map G.issueNumber is
  erepo <- userRepo owner proj
  case erepo of
    Left err -> do
      putStrLn $ "Error: " ++ show err
      return $ Left $ show err
    Right repo -> let
      mlang = repoLanguage repo
      in return $ Right $ Project issues langs S.empty (repoSize repo) (-1) (repoWatchers repo)

