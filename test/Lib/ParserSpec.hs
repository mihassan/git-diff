{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib.ParserSpec (spec) where

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Test.Hspec
import Test.Hspec.Megaparsec
import Lib

spec :: Spec
spec = do
  describe "chunkP Parser" $ do
    it "should parse valid chunk" $ do
      parse chunkP "" ("@@ -1,6 +1,6 @@\n Unchanged\n+Added\n-Removed" :: Text)
        `shouldParse`
          Chunk (LineRange 1 6) (LineRange 1 6) [ContextLine "Unchanged", AddedLine "Added", RemovedLine "Removed"]

    it "should parse valid chunk with newline" $ do
      parse chunkP "" ("@@ -1,6 +1,6 @@\n Unchanged\n+Added\n-Removed\n" :: Text)
        `shouldParse`
          Chunk (LineRange 1 6) (LineRange 1 6) [ContextLine "Unchanged", AddedLine "Added", RemovedLine "Removed"]

    it "should parse valid chunk with more non-chunk lines" $ do
      parse chunkP "" ("@@ -1,6 +1,6 @@\n Unchanged\n+Added\n-Removed\nend of chunk\n" :: Text)
        `shouldParse`
          Chunk (LineRange 1 6) (LineRange 1 6) [ContextLine "Unchanged", AddedLine "Added", RemovedLine "Removed"]

    it "should fail with empty input" $ do
      parse chunkP "" `shouldFailOn` ("" :: Text)

    it "should fail with invalid chunk header" $ do
      parse chunkP "" `shouldFailOn` ("-1,6 +1,6\n Unchanged" :: Text)

    it "should fail with no chunk header" $ do
     parse chunkP "" `shouldFailOn` ("+Added\n Unchanged" :: Text)

    it "should fail with no diff line" $ do
      parse chunkP "" `shouldFailOn` ("@@ -1,6 +1,6 @@" :: Text)

  describe "lineRangeP Parser" $ do
    it "should parse single line range with minus prefix" $ do
      parse lineRangeP "" ("-15" :: Text)
        `shouldParse`
        LineRange 15 1

    it "should parse single line range with plus prefix" $ do
      parse lineRangeP "" ("+15" :: Text)
        `shouldParse`
        LineRange 15 1

    it "should parse multi line range with minus prefix" $ do
      parse lineRangeP "" ("-15,5" :: Text)
        `shouldParse`
        LineRange 15 5

    it "should parse multi line range with plus prefix" $ do
      parse lineRangeP "" ("+15,5" :: Text)
        `shouldParse`
        LineRange 15 5


    it "should fail without minus or plus prefix" $ do
      parse lineRangeP ""
        `shouldFailOn`
        ("15,5" :: Text)

    it "should fail with letter" $ do
      parse lineRangeP ""
        `shouldFailOn`
        ("range 15,5" :: Text)

    it "should fail with floating point number" $ do
      parse lineRangeP ""
        `shouldFailOn`
        ("+.5,5" :: Text)

    it "should fail with space prefix" $ do
      parse lineRangeP ""
        `shouldFailOn`
        (" 15,5" :: Text)

  describe "lineDiffP Parser" $ do
    it "should parse added line" $ do
      parse lineDiffP "" ("+Added Line" :: Text)
        `shouldParse`
        AddedLine "Added Line"

    it "should parse removed line" $ do
      parse lineDiffP "" ("-Removed Line" :: Text)
        `shouldParse`
        RemovedLine "Removed Line"

    it "should parse context line" $ do
      parse lineDiffP "" (" Unchanged Line" :: Text)
        `shouldParse`
        ContextLine "Unchanged Line"


    it "should fail with line starting with letter" $ do
      parse lineDiffP ""
        `shouldFailOn`
        ("Invalid Line" :: Text)

    it "should fail with line starting with symbol" $ do
      parse lineDiffP ""
        `shouldFailOn`
        ("=Invalid Line" :: Text)

    it "should fail with line starting with digit" $ do
      parse lineDiffP ""
        `shouldFailOn`
        ("0Invalid Line" :: Text)

    it "should fail with empty line" $ do
      parse lineDiffP ""
        `shouldFailOn`
        ("" :: Text)
