{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe
import Data.Text.IO as T
import Lib

main :: IO ()
main = T.interact $ fromJust . parseGitDiff
