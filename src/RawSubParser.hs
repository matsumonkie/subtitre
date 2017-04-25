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

parseSubtitles :: Text -> Either ParseError [RawSubCtx]
parseSubtitles = parse subtitles "game of thrones"

subtitles :: Parsec Text () [RawSubCtx]
subtitles = do
  subtitles <- (subtitleCtx `sepBy` (string "\n\n"))  <?> "B"
  eof <?> "C"
  return subtitles

subtitleCtx :: Parsec Text () RawSubCtx
subtitleCtx = do
  sequence <- read <$> many1 digit  <?> "I"
  newline  <?> "G"
  timingCtx <- timingCtx  <?> "J"
  newline <?> "H"
  lines <- many1 (sentence <* optional endOfLine) <?> "A"
  return $ SubCtx sequence timingCtx lines

sentence :: Parsec Text () Text
sentence = do
  pack <$> many1 (noneOf "\n\r")  <?> "E"

timingCtx :: Parsec Text () TimingCtx
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
    twoDigits = read <$> Text.Parsec.count 2 digit
    threeDigits = read <$> Text.Parsec.count 3 digit
    colon = char ':'
    comma = char ','
    separator = (string " --> " <?> "wrong timingCtx separator")
