module Parser (
  subtitlesParser
) where

import Type
import qualified Text.Parsec as Parsec
import qualified Data.Text as T

subtitlesParser :: Parsec.Parsec String () [SubtitleCtx]
subtitlesParser = do
  subtitles <- Parsec.many subtitleParser
  return subtitles

subtitleParser :: Parsec.Parsec String () SubtitleCtx
subtitleParser = do
  seq <- read <$> Parsec.many1 Parsec.digit
  Parsec.newline
  timingCtx <- timingParser
  Parsec.newline
  sentences <- Parsec.manyTill sentenceParser Parsec.newline
  return $ SubtitleCtx seq timingCtx sentences

sentenceParser :: Parsec.Parsec String () T.Text
sentenceParser = do
  sentence <- Parsec.manyTill Parsec.anyChar Parsec.newline
  return $ T.pack sentence

-- 00:00:26,722 --> 00:00:29,023
timingParser :: Parsec.Parsec String () TimingCtx
timingParser = do
  bhour <- twoDigits
  colon
  bmin <- twoDigits
  colon
  bsec <- twoDigits
  comma
  bmsec <- threeDigits
  separator
  ehour <- twoDigits
  colon
  emin <- twoDigits
  colon
  esec <- twoDigits
  comma
  emsec <- threeDigits
  return $ TimingCtx (Timing bhour bmin bsec bmsec) (Timing ehour emin esec emsec)
  where
    twoDigits = read <$> Parsec.count 2 Parsec.digit
    threeDigits = read <$> Parsec.count 3 Parsec.digit
    colon = Parsec.char ':'
    comma = Parsec.char ','
    separator = Parsec.string " --> "
