{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib.ParserSpec (spec) where

import Text.Megaparsec
import Test.Hspec
import Test.Hspec.Megaparsec
import Lib

spec :: Spec
spec = do
  describe "diffP Parser" $ do
    it "should parse valid git diff without ending with newline" $ do
      parse diffP "" "diff\nindex\n--- a/f1\n+++ b/f2\n@@ -1 +2 @@\n-A\n+B"
        `shouldParse`
        Diff [FileDiff ["index"] "f1" "f2" [Chunk (LineRange 1 1) (LineRange 2 1) [RemovedLine "A", AddedLine "B"]]]

    it "should parse valid git diff ending with newline" $ do
      parse diffP "" "diff\nindex\n--- a/f1\n+++ b/f2\n@@ -1 +2 @@\n-A\n+B\n"
        `shouldParse`
        Diff [FileDiff ["index"] "f1" "f2" [Chunk (LineRange 1 1) (LineRange 2 1) [RemovedLine "A", AddedLine "B"]]]

    it "should parse valid git diff with multiple files" $ do
      parse diffP "" "diff\nindex\n--- a/f1\n+++ b/f2\n@@ -1 +2 @@\n-A\n+B\ndiff\nindex\n--- a/f1\n+++ b/f2\n@@ -1 +2 @@\n-A\n+B\n"
        `shouldParse`
        Diff [
          FileDiff ["index"] "f1" "f2" [Chunk (LineRange 1 1) (LineRange 2 1) [RemovedLine "A", AddedLine "B"]],
          FileDiff ["index"] "f1" "f2" [Chunk (LineRange 1 1) (LineRange 2 1) [RemovedLine "A", AddedLine "B"]]
        ]

  describe "fileDiffP Parser" $ do
    it "should pass valid file diff section without ending with newline" $ do
      parse fileDiffP "" "diff\nindex\n--- a/f1\n+++ b/f2\n@@ -1 +2 @@\n-A\n+B"
        `shouldParse`
        FileDiff ["index"] "f1" "f2" [Chunk (LineRange 1 1) (LineRange 2 1) [RemovedLine "A", AddedLine "B"]]

    it "should pass valid file diff section ending with newline" $ do
      parse fileDiffP "" "diff\nindex\n--- a/f1\n+++ b/f2\n@@ -1 +2 @@\n-A\n+B\n"
        `shouldParse`
        FileDiff ["index"] "f1" "f2" [Chunk (LineRange 1 1) (LineRange 2 1) [RemovedLine "A", AddedLine "B"]]

    it "should pass valid file diff section with multiple chunks" $ do
      parse fileDiffP "" "diff\nindex\n--- a/f1\n+++ b/f2\n@@ -1 +2 @@\n-A\n+B\n@@ -5,2 +5,3 @@\n U\n+C"
        `shouldParse`
        FileDiff ["index"] "f1" "f2" [
          Chunk (LineRange 1 1) (LineRange 2 1) [RemovedLine "A", AddedLine "B"],
          Chunk (LineRange 5 2) (LineRange 5 3) [ContextLine "U", AddedLine "C"]
        ]

  describe "headerP Parser" $ do
    it "should parse valid header without ending with newline" $ do
      parse headerP "" "index 12..34 56" `shouldParse` "index 12..34 56"

    it "should parse valid header ending with newline" $ do
      parse headerP "" "index 12..34 56\n" `shouldParse` "index 12..34 56"


    it "should fail on empty line" $ do
      parse headerP "" `shouldFailOn` ""

    it "should fail on newline" $ do
      parse headerP "" `shouldFailOn` "\n"

    it "should fail on line starting with '--- '" $ do
      parse headerP "" `shouldFailOn` "--- Text"

  describe "chunkP Parser" $ do
    it "should parse valid chunk" $ do
      parse chunkP "" "@@ -1,6 +1,6 @@\n Unchanged\n+Added\n-Removed"
        `shouldParse`
          Chunk (LineRange 1 6) (LineRange 1 6) [ContextLine "Unchanged", AddedLine "Added", RemovedLine "Removed"]

    it "should parse valid chunk with newline" $ do
      parse chunkP "" "@@ -1,6 +1,6 @@\n Unchanged\n+Added\n-Removed\n"
        `shouldParse`
          Chunk (LineRange 1 6) (LineRange 1 6) [ContextLine "Unchanged", AddedLine "Added", RemovedLine "Removed"]

    it "should parse valid chunk with more non-chunk lines" $ do
      parse chunkP "" "@@ -1,6 +1,6 @@\n Unchanged\n+Added\n-Removed\nend of chunk\n"
        `shouldParse`
          Chunk (LineRange 1 6) (LineRange 1 6) [ContextLine "Unchanged", AddedLine "Added", RemovedLine "Removed"]

    it "should fail with empty input" $ do
      parse chunkP "" `shouldFailOn` ""

    it "should fail with invalid chunk header" $ do
      parse chunkP "" `shouldFailOn` "-1,6 +1,6\n Unchanged"

    it "should fail with single line range" $ do
      parse chunkP "" `shouldFailOn` "@@ -1,6 @@\n Unchanged"

    it "should fail with more than two line ranges" $ do
      parse chunkP "" `shouldFailOn` "@@ -1,6 +1,6 +1,16 @@\n Unchanged"

    it "should fail with no chunk header" $ do
     parse chunkP "" `shouldFailOn` "+Added\n Unchanged"

    it "should fail with no diff line" $ do
      parse chunkP "" `shouldFailOn` "@@ -1,6 +1,6 @@"


  describe "chunkHeaderP Parser" $ do
    it "should parse valid chunk header" $ do
      parse chunkHeaderP "" "@@ -1,6 +1,5 @@\n" `shouldParse` [LineRange 1 6, LineRange 1 5]

    it "should fail on invalid chunk header" $ do
      parse chunkHeaderP "" `shouldFailOn` "-1,6 +1,5\n"


  describe "lineRangeP Parser" $ do
    it "should parse single line range with minus prefix" $ do
      parse lineRangeP "" "-15" `shouldParse` LineRange 15 1

    it "should parse single line range with plus prefix" $ do
      parse lineRangeP "" "+15" `shouldParse` LineRange 15 1

    it "should parse multi line range with minus prefix" $ do
      parse lineRangeP "" "-15,5" `shouldParse` LineRange 15 5

    it "should parse multi line range with plus prefix" $ do
      parse lineRangeP "" "+15,5" `shouldParse` LineRange 15 5


    it "should fail without minus or plus prefix" $ do
      parse lineRangeP "" `shouldFailOn` "15,5"

    it "should fail with letter" $ do
      parse lineRangeP "" `shouldFailOn` "range 15,5"

    it "should fail with floating point number" $ do
      parse lineRangeP "" `shouldFailOn` "+.5,5"

    it "should fail with space prefix" $ do
      parse lineRangeP "" `shouldFailOn` " 15,5"

  describe "lineDiffP Parser" $ do
    it "should parse added line" $ do
      parse lineDiffP "" "+Added Line" `shouldParse` AddedLine "Added Line"

    it "should parse removed line" $ do
      parse lineDiffP "" "-Removed Line" `shouldParse` RemovedLine "Removed Line"

    it "should parse context line" $ do
      parse lineDiffP "" " Unchanged Line" `shouldParse` ContextLine "Unchanged Line"


    it "should fail with line starting with letter" $ do
      parse lineDiffP "" `shouldFailOn` "Invalid Line"

    it "should fail with line starting with symbol" $ do
      parse lineDiffP "" `shouldFailOn` "=Invalid Line"

    it "should fail with line starting with digit" $ do
      parse lineDiffP "" `shouldFailOn` "0Invalid Line"

    it "should fail with empty line" $ do
      parse lineDiffP "" `shouldFailOn` ""
