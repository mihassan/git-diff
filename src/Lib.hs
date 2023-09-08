{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Applicative
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Aeson
import Data.Aeson.Encode.Pretty
import GHC.Generics

-- * Type declarations.

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
diffP = Diff <$> many1 fileDiffP

fileDiffP :: Parser FileDiff
fileDiffP
  =   FileDiff
  <$> (line *> many1 headerP)
  <*> ("--- a/" *> line)
  <*> ("+++ b/" *> line)
  <*> many1 chunkP

headerP :: Parser Text
headerP = notFollowedBy "--- " *> line

chunkP :: Parser Chunk
chunkP
  =   Chunk
  <$> ("@@ " *> lineRangeP <* char ' ')
  <*> (lineRangeP <* " @@" <* endOfLine)
  <*> (many1 lineDiffP)

lineRangeP :: Parser LineRange
lineRangeP
  =   LineRange
  <$> (("-" <|> "+") *> integer)
  <*> (("," *> integer) <|> pure 1)

lineDiffP :: Parser LineDiff
lineDiffP
  =   ("+" *> (AddedLine <$> line))
  <|> ("-" *> (RemovedLine <$> line))
  <|> (" " *> (ContextLine <$> line))

parseGitDiff :: Text -> Maybe Text
parseGitDiff t = case parseOnly diffP t of
  Left _ -> Nothing
  Right v -> Just . prettyPrint $ toJSON v
  where
    config = defConfig { confIndent = Spaces 2, confTrailingNewline = True }
    prettyPrint = toStrict . toLazyText . encodePrettyToTextBuilder' config

-- * Helper parsers.

notFollowedBy :: Show a => Parser a -> Parser ()
-- notFollowedBy p = optional p >>= guard . isNothing
notFollowedBy p = do
  m <- optional p
  case m of
    Nothing -> pure ()
    Just c -> fail $ "notFollowedBy " <> show c

integer :: Parser Int
integer = read <$> many1 digit

line :: Parser Text
line = takeWhile1 (not . isEndOfLine) <* (endOfLine <|> endOfInput)

