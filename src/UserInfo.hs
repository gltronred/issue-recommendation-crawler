{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module UserInfo where

import Types

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import qualified Data.Set as S
import Github.Data.Definitions
import Github.Repos
import qualified Github.Issues as G

userInfo = undefined