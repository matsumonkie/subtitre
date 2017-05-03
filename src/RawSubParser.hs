{-# LANGUAGE OverloadedStrings #-}
{-
  Take an .srt file as an input and deserialize it
-}

module RawSubParser (
  parseSubtitles
) where

import Type
import Text.Parsec.Combinator
import Data.Text
import Text.Parsec
import Data.Functor.Identity
import Data.Monoid
import Control.Monad

parseSubtitles :: Text -> Either ParseError [RawSubCtx]
parseSubtitles = parse subtitles "game of thrones"

subtitles :: Parsec Text () [RawSubCtx]
subtitles = do
  subtitles <- subtitleCtxP `sepBy` endOfLine
  eof
  return subtitles

subtitleCtxP :: Parsec Text () RawSubCtx
subtitleCtxP = do
  sequence <- sequenceP
  newline
  timingCtx <- timingCtxP
  newline
  lines <- sentenceP `sepEndBy` endOfLine
  return $ SubCtx sequence timingCtx lines

sequenceP :: Parsec Text () Sequence
sequenceP =
  read <$> many1 digit <?> "RawSubParser sequence"

sentenceP :: Parsec Text () Text
sentenceP = do
  pack <$> many1 (noneOf "\n\r") <?> "RawSubParser sentence"

timingCtxP :: Parsec Text () TimingCtx
timingCtxP = do
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
    twoDigits = read <$> Text.Parsec.count 2 digit
    threeDigits = read <$> Text.Parsec.count 3 digit
    colon = char ':'
    comma = char ','
    separator = (string " --> " <?> "RawSubParser timingCtx separator")
