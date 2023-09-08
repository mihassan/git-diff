{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib.ParserSpec (spec) where

import Data.Text
import Test.Hspec
import Test.Hspec.Attoparsec
import Lib

spec :: Spec
spec = do
  describe "chunkP Parser" $ do
    it "should parse valid chunk" $ do
      ("@@ -1,6 +1,6 @@\n Unchanged\n+Added\n-Removed" :: Text) ~> chunkP
        `shouldParse`
          Chunk (LineRange 1 6) (LineRange 1 6) [ContextLine "Unchanged", AddedLine "Added", RemovedLine "Removed"]
    it "should parse valid chunk with newline" $ do
      ("@@ -1,6 +1,6 @@\n Unchanged\n+Added\n-Removed\n" :: Text) ~> chunkP
        `shouldParse`
          Chunk (LineRange 1 6) (LineRange 1 6) [ContextLine "Unchanged", AddedLine "Added", RemovedLine "Removed"]
    it "should parse valid chunk with more non-chunk lines" $ do
      ("@@ -1,6 +1,6 @@\n Unchanged\n+Added\n-Removed\nend of chunk\n" :: Text) ~> chunkP
        `shouldParse`
          Chunk (LineRange 1 6) (LineRange 1 6) [ContextLine "Unchanged", AddedLine "Added", RemovedLine "Removed"]

    it "should fail with empty input" $ do
      chunkP `shouldFailOn` ("" :: Text)
    it "should fail with invalid chunk header" $ do
      chunkP `shouldFailOn` ("-1,6 +1,6\n Unchanged" :: Text)
    it "should fail with no chunk header" $ do
      chunkP `shouldFailOn` ("+Added\n Unchanged" :: Text)
    it "should fail with no diff line" $ do
      chunkP `shouldFailOn` ("@@ -1,6 +1,6 @@" :: Text)
  describe "lineRangeP Parser" $ do
    it "should parse single line range with minus prefix" $ do
      ("-15" :: Text) ~> lineRangeP `shouldParse` LineRange 15 1
    it "should parse single line range with plus prefix" $ do
      lineRangeP `shouldSucceedOn` ("+15" :: Text)
      ("+15" :: Text) ~> lineRangeP `shouldParse` LineRange 15 1
    it "should parse multi line range with minus prefix" $ do
      ("-15,5" :: Text) ~> lineRangeP `shouldParse` LineRange 15 5
    it "should parse multi line range with plus prefix" $ do
      ("+15,5" :: Text) ~> lineRangeP `shouldParse` LineRange 15 5

    it "should fail without minus or plus prefix" $ do
      lineRangeP `shouldFailOn` ("15,5" :: Text)
    it "should fail with letter" $ do
      lineRangeP `shouldFailOn` ("range 15,5" :: Text)
    it "should fail with floating point number" $ do
      lineRangeP `shouldFailOn` ("+.5,5" :: Text)
    it "should fail with space prefix" $ do
      lineRangeP `shouldFailOn` (" 15,5" :: Text)

  describe "lineDiffP Parser" $ do
    it "should parse added line" $ do
      ("+Added Line" :: Text) ~> lineDiffP `shouldParse` AddedLine "Added Line"
    it "should parse removed line" $ do
      ("-Removed Line" :: Text) ~> lineDiffP `shouldParse` RemovedLine "Removed Line"
    it "should parse context line" $ do
      (" Unchanged Line" :: Text) ~> lineDiffP `shouldParse` ContextLine "Unchanged Line"

    it "should fail with line starting with letter" $ do
      lineDiffP `shouldFailOn` ("Invalid Line" :: Text)
    it "should fail with line starting with symbol" $ do
      lineDiffP `shouldFailOn` ("=Invalid Line" :: Text)
    it "should fail with line starting with digit" $ do
      lineDiffP `shouldFailOn` ("0Invalid Line" :: Text)
    it "should fail with empty line" $ do
      lineDiffP `shouldFailOn` ("" :: Text)
