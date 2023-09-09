{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib.HelperParserSpec (spec) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Test.Hspec
import Test.Hspec.Megaparsec
import Lib

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

    it "should parse multiple lines ending with newline" $ do
      parse (some line) "" "Line1\nLine2\n" `shouldParse` ["Line1", "Line2"]

    it "should parse multiple lines not ending with newline" $ do
      parse (some line) "" "Line1\nLine2" `shouldParse` ["Line1", "Line2"]


    it "should fail on empty line not ending with newline" $ do
      parse line "" `shouldFailOn` ""

    it "should fail on empty line ending with newline" $ do
      parse line "" `shouldFailOn` "\n"

    it "should fail on following another parser that consumes everything ending with newline" $ do
      parse (some letterChar *> line) "" `shouldFailOn` "FirstParser\n"

    it "should fail on following another parser that consumes everything not ending with newline" $ do
      parse (some letterChar *> line) "" `shouldFailOn` "FirstParser"
