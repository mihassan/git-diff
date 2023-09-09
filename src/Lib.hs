{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Void
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics

-- * Type declarations.

type Parser = Parsec Void Text

newtype Diff = Diff [FileDiff] deriving (Show, Eq, Generic)

data FileDiff = FileDiff {
  headers :: [Text],
  fromFile :: Text,
  toFile :: Text,
  chunks :: [Chunk]
} deriving (Show, Eq, Generic)

data Chunk = Chunk {
  fromLineRange :: LineRange,
  toLineRange :: LineRange,
  lineDiffs :: [LineDiff]
} deriving (Show, Eq, Generic)

data LineRange = LineRange {
  from :: Int,
  length :: Int
} deriving (Show, Eq, Generic)

data LineDiff = AddedLine Text | RemovedLine Text | ContextLine Text deriving (Show, Eq, Generic)

-- * JSON instances.

instance ToJSON Diff
instance ToJSON FileDiff
instance ToJSON Chunk
instance ToJSON LineRange
instance ToJSON LineDiff where
  toJSON = genericToJSON defaultOptions { sumEncoding = ObjectWithSingleField }

-- * Parsers.

diffP :: Parser Diff
diffP = Diff <$> some fileDiffP

fileDiffP :: Parser FileDiff
fileDiffP
  =   FileDiff
  <$> (line *> some headerP)
  <*> ("--- a/" *> line)
  <*> ("+++ b/" *> line)
  <*> some chunkP

headerP :: Parser Text
headerP = notFollowedBy "--- " *> line

chunkP :: Parser Chunk
chunkP
  =   Chunk
  <$> ("@@ " *> lineRangeP <* " ")
  <*> (lineRangeP <* " @@" <* eol)
  <*> (some lineDiffP)

lineRangeP :: Parser LineRange
lineRangeP
  =   LineRange
  <$> (("-" <|> "+") *> decimal)
  <*> (("," *> decimal) <|> pure 1)

lineDiffP :: Parser LineDiff
lineDiffP
  =   ("+" *> (AddedLine <$> line))
  <|> ("-" *> (RemovedLine <$> line))
  <|> (" " *> (ContextLine <$> line))

parseGitDiff :: Text -> Maybe Text
parseGitDiff t = case parse diffP "GitDiff" t of
  Left _ -> Nothing
  Right v -> Just . prettyPrint $ toJSON v
  where
    config = defConfig { confIndent = Spaces 2, confTrailingNewline = True }
    prettyPrint = toStrict . toLazyText . encodePrettyToTextBuilder' config

-- * Helper parsers.
line :: Parser Text
line = pack <$> someTill anySingle ((eol *> pure ()) <|> eof)

