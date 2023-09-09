{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib.HelperParserSpec (spec) where

import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char
import Test.Hspec
import Test.Hspec.Megaparsec
import Lib

spec :: Spec
spec = do
  describe "line Parser" $ do
    it "should succeed on non-empty line ending with newline" $ do
      parse line ""
        `shouldSucceedOn`
        ("A simple line.\n" :: Text)

    it "should succeed on non-empty line not ending with newline" $ do
      parse line ""
        `shouldSucceedOn`
        ("A simple line." :: Text)

    it "should succeed on multiple non-empty lines" $ do
      parse line ""
        `shouldSucceedOn`
        ("A simple line.\nA new line\n" :: Text)

    it "should succeed following another parser" $ do
      parse (some letterChar *> line) ""
        `shouldSucceedOn`
        ("A simple line.\n" :: Text)

    it "should succeed consuming newline between two parsers" $ do
      parse (some letterChar *> line *> some letterChar) ""
        `shouldSucceedOn`
        ("First rest of line\nSecond" :: Text)

    it "should succeed with multiple lines" $ do
      parse (some line) ""
        `shouldSucceedOn`
        ("A simple line.\nA new line\nAnother line" :: Text)
