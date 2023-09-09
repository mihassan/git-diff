{-# LANGUAGE OverloadedStrings #-}

module Lib.ParserGoldenSpec (spec) where

import qualified Data.Text as T
import Lib
import Test.Hspec
import Test.Hspec.Golden

spec :: Spec
spec = do
  describe "parseGitDiff" $ do
    it "parses valid git diff correctly" $ do
      let input =
            T.unlines
              [ "diff --git a/file1.txt b/file1.txt",
                "index 168ed52..e41a4f5 100644",
                "--- a/file1.txt",
                "+++ b/file1.txt",
                "@@ -1,6 +1,6 @@",
                " This is a sample file.",
                "-This line is changed.",
                "+These lines are part of the original content.",
                " Add a new line here.",
                " Another change in content.",
                "-Additional lines in the middle.",
                "+More lines in the original file.",
                " New content at the end of the file.",
                "diff --git a/file2.txt b/file2.txt",
                "index 112ed52..e21a1f5 199231",
                "--- a/file2.txt",
                "+++ b/file2.txt",
                "@@ -1 +10,2 @@",
                " This is a sample file.",
                "-This line is changed.",
                "+These lines are part of the original content.",
                " Add a new line here.",
                " Another change in content.",
                "-Additional lines in the middle.",
                "+More lines in the original file."
              ]
      let Right actual = parseGitDiff input
      defaultGolden "parseGitDiff" (T.unpack actual)
