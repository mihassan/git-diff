{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Lib.HelperParserSpec (spec) where

import Data.Attoparsec.Text
import Data.Text
import Test.Hspec
import Test.Hspec.Attoparsec
import Lib

spec :: Spec
spec = do
  describe "notFollowedBy Parser" $ do
    it "should succeed on empty input" $ do
      notFollowedBy "xyz" `shouldSucceedOn` ("" :: Text)
    it "should succeed on mismatch" $ do
      notFollowedBy "xyz" `shouldSucceedOn` ("abc" :: Text)
    it "should succeed on partial match" $ do
      notFollowedBy "xyz" `shouldSucceedOn` ("xy" :: Text)

    it "should fail on given parser" $ do
      notFollowedBy "xyz" `shouldFailOn` ("xyz" :: Text)
    it "should fail on given parser with extra input" $ do
      notFollowedBy "xyz" `shouldFailOn` ("xyzabc" :: Text)

    it "should leave input uncosumed on mismatch" $ do
      ("xyz" :: Text) ~?> notFollowedBy "abc" `leavesUnconsumed` "xyz"
    it "should leave input uncosumed on partial match" $ do
      ("xyz" :: Text) ~?> notFollowedBy "xyZ" `leavesUnconsumed` "xyz"

    it "should succeed with following string parser" $ do
      (notFollowedBy "xyz" *> "abc") `shouldSucceedOn` ("abc" :: Text)
    it "should succeed with following generic parser" $ do
      (notFollowedBy "xyz" *> many1 letter) `shouldSucceedOn` ("abc" :: Text)

    it "should fail with following string parser" $ do
      (notFollowedBy "xyz" *> "abc") `shouldFailOn` ("xyzabc" :: Text)
    it "should fail with following generic parser" $ do
      (notFollowedBy "xyz" *> many1 letter) `shouldFailOn` ("xyzabc" :: Text)

    it "should succeed after string parser" $ do
      ("xyz" <* notFollowedBy "xyz") `shouldSucceedOn` ("xyz" :: Text)
    it "should succeed after generic parser" $ do
      (many1 letter <* notFollowedBy "xyz") `shouldSucceedOn` ("xyz" :: Text)
    it "should succeed within repeated parser" $ do
      (many1 (notFollowedBy (decimal @Integer) *> many1 letter)) `shouldSucceedOn` ("xyz abc 123 rest" :: Text)

  describe "line Parser" $ do
    it "should succeed on non-empty line ending with newline" $ do
      line `shouldSucceedOn` ("A simple line.\n" :: Text)
    it "should succeed on non-empty line not ending with newline" $ do
      line `shouldSucceedOn` ("A simple line." :: Text)
    it "should succeed on multiple non-empty lines" $ do
      line `shouldSucceedOn` ("A simple line.\nA new line\n" :: Text)
    it "should succeed following another parser" $ do
      (many1 letter *> line) `shouldSucceedOn` ("A simple line.\n" :: Text)
    it "should succeed consuming newline between two parsers" $ do
      (many1 letter *> line *> many1 letter) `shouldSucceedOn` ("First rest of line\nSecond" :: Text)
    it "should succeed with multiple lines" $ do
      many1 line `shouldSucceedOn` ("A simple line.\nA new line\nAnother line" :: Text)
