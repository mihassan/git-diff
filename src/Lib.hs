{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Functor
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Void
import GHC.Generics
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (decimal)

-- * Type declarations.

type Parser = Parsec Void Text

newtype Diff = Diff [FileDiff] deriving (Show, Eq, Generic)

data FileDiff = FileDiff
  { headers :: [Text],
    fromFile :: Text,
    toFile :: Text,
    chunks :: [Chunk]
  }
  deriving (Show, Eq, Generic)

data Chunk = Chunk
  { fromLineRange :: LineRange,
    toLineRange :: LineRange,
    lineDiffs :: [LineDiff]
  }
  deriving (Show, Eq, Generic)

data LineRange = LineRange
  { from :: Int,
    length :: Int
  }
  deriving (Show, Eq, Generic)

data LineDiff = AddedLine Text | RemovedLine Text | ContextLine Text deriving (Show, Eq, Generic)

-- * JSON instances.

instance ToJSON Diff

instance ToJSON FileDiff

instance ToJSON Chunk

instance ToJSON LineRange

instance ToJSON LineDiff where
  toJSON = genericToJSON defaultOptions {sumEncoding = ObjectWithSingleField}

-- * Parsers.

diffP :: Parser Diff
diffP = Diff <$> some fileDiffP

fileDiffP :: Parser FileDiff
fileDiffP =
  FileDiff
    <$> (line1 *> some headerP)
    <*> ("--- a/" *> line1)
    <*> ("+++ b/" *> line1)
    <*> some chunkP

headerP :: Parser Text
headerP = notFollowedBy "--- " *> line1

chunkP :: Parser Chunk
chunkP = do
  [f, t] <- chunkHeaderP
  ls <- some lineDiffP
  pure $ Chunk f t ls

chunkHeaderP :: Parser [LineRange]
chunkHeaderP = "@@" *> space *> lineRangeP `endBy1` space <* "@@" <* eol

lineRangeP :: Parser LineRange
lineRangeP =
  LineRange
    <$> (("-" <|> "+") *> decimal)
    <*> (("," *> decimal) <|> pure 1)

lineDiffP :: Parser LineDiff
lineDiffP =
  ("+" *> (AddedLine <$> line))
    <|> ("-" *> (RemovedLine <$> line))
    <|> (" " *> (ContextLine <$> line))

parseGitDiff :: Text -> Either String Text
parseGitDiff t = case parse diffP "GitDiff" t of
  Left e -> Left $ errorBundlePretty e
  Right v -> Right . prettyPrint $ toJSON v
  where
    config = defConfig {confIndent = Spaces 2, confTrailingNewline = True}
    prettyPrint = toStrict . toLazyText . encodePrettyToTextBuilder' config

-- * Helper parsers.

-- | Parse end of line or end of file.
eol' :: Parser ()
eol' = eof <|> eol $> ()

-- | Parse rest of the line. It consumes new line character but does not include in the returned value.
-- Return empty on empty line.
line :: Parser Text
line = pack <$> manyTill (noneOf ['\r', '\n']) eol'

-- | Parse rest of the line. It consumes new line character but does not include in the returned value.
-- Fails on empty line.
line1 :: Parser Text
line1 = pack <$> someTill (noneOf ['\r', '\n']) eol'
