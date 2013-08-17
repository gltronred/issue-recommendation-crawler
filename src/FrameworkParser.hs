{-# LANGUAGE OverloadedStrings, DeriveGeneric, ImplicitParams #-}

module FrameworkParser (frameworksFor) where

import Types
import Network

import qualified Data.ByteString.Char8 as BS
import qualified Data.Set as S

frameworks :: [Framework]
frameworks = []

frameworksFor :: BS.ByteString -> BS.ByteString -> IO (S.Set BS.ByteString)
frameworksFor owner proj = return $ S.empty

