{-
  Take an .srt file as an input and deserialize it
-}

module RawSubParser (
  parseSubtitles
) where

import Type
import Text.Parsec.Combinator
import qualified Data.Text as T
import Text.Parsec

parseSubtitles :: T.Text -> Either ParseError [RawSubCtx]
parseSubtitles = parse subtitles "game of thrones"

subtitles :: Parsec T.Text () [RawSubCtx]
subtitles = do
  subtitles <- subtitleCtx `sepBy` endOfLine
  eof
  return subtitles

subtitleCtx :: Parsec T.Text () RawSubCtx
subtitleCtx = do
  sequence <- read <$> many1 digit
  newline
  timingCtx <- timingCtx
  newline
  lines <- sentence `endBy` endOfLine
  return $ SubCtx sequence timingCtx lines

sentence :: Parsec T.Text () T.Text
sentence = do
  T.pack <$> many1 (noneOf "\n\r")

timingCtx :: Parsec T.Text () TimingCtx
timingCtx = do
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
    twoDigits = read <$> count 2 digit
    threeDigits = read <$> count 3 digit
    colon = char ':'
    comma = char ','
    separator = (string " --> " <?> "wrong timingCtx separator")
