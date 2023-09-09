{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib.HelperParserSpec (spec) where

import Lib
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: Spec
spec = do
  describe "eol' Parser" $ do
    it "should succeed on newline character" $ do
      parse eol' "" `shouldSucceedOn` "\n"

    it "should succeed on crlf character" $ do
      parse eol' "" `shouldSucceedOn` "\r\n"

    it "should succeed on eof" $ do
      parse eol' "" `shouldSucceedOn` ""

    it "should fail on carriage return character" $ do
      parse eol' "" `shouldFailOn` "\r"

    it "should fail on any string befoew crlf" $ do
      parse eol' "" `shouldFailOn` "Some Text\n"

    it "should fail on any letter" $ do
      parse eol' "" `shouldFailOn` "Some Text"

    it "should fail on space" $ do
      parse eol' "" `shouldFailOn` " "

  describe "line Parser" $ do
    it "should parse non-empty line ending with newline" $ do
      parse line "" "Line\n" `shouldParse` "Line"

    it "should parse non-empty line not ending with newline" $ do
      parse line "" "Line" `shouldParse` "Line"

    it "should parse first line from multiple non-empty lines" $ do
      parse line "" "Line1\nLine2\n" `shouldParse` "Line1"

    it "should parse following another parser" $ do
      parse (some letterChar *> space *> line) "" "First Last\n" `shouldParse` "Last"

    it "should parse consuming newline between two parsers" $ do
      parse (some letterChar *> line *> some letterChar) "" "First Rest\nSecond" `shouldParse` "Second"

    it "should parse empty line not ending with newline" $ do
      parse line "" "" `shouldParse` ""

    it "should parse empty line ending with newline" $ do
      parse line "" "\n" `shouldParse` ""

    it "should parse following another parser that consumes everything ending with newline" $ do
      parse (some letterChar *> line) "" "FirstParser\n" `shouldParse` ""

    it "should parse following another parser that consumes everything not ending with newline" $ do
      parse (some letterChar *> line) "" "FirstParser" `shouldParse` ""

  describe "line1 Parser" $ do
    it "should parse non-empty line ending with newline" $ do
      parse line1 "" "Line\n" `shouldParse` "Line"

    it "should parse non-empty line not ending with newline" $ do
      parse line1 "" "Line" `shouldParse` "Line"

    it "should parse first line from multiple non-empty lines" $ do
      parse line1 "" "Line1\nLine2\n" `shouldParse` "Line1"

    it "should parse following another parser" $ do
      parse (some letterChar *> space *> line1) "" "First Last\n" `shouldParse` "Last"

    it "should parse consuming newline between two parsers" $ do
      parse (some letterChar *> line1 *> some letterChar) "" "First Rest\nSecond" `shouldParse` "Second"

    it "should parse multiple lines ending with newline" $ do
      parse (some line1) "" "Line1\nLine2\n" `shouldParse` ["Line1", "Line2"]

    it "should parse multiple lines not ending with newline" $ do
      parse (some line1) "" "Line1\nLine2" `shouldParse` ["Line1", "Line2"]

    it "should fail on empty line not ending with newline" $ do
      parse line1 "" `shouldFailOn` ""

    it "should fail on empty line ending with newline" $ do
      parse line1 "" `shouldFailOn` "\n"

    it "should fail on following another parser that consumes everything ending with newline" $ do
      parse (some letterChar *> line1) "" `shouldFailOn` "FirstParser\n"

    it "should fail on following another parser that consumes everything not ending with newline" $ do
      parse (some letterChar *> line1) "" `shouldFailOn` "FirstParser"
