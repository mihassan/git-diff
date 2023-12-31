{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text.IO as T
import Lib

main :: IO ()
main = T.interact $ either error id . parseGitDiff
