module Parser (
  parseSubtitles
) where

import Type
import Text.Parsec.Combinator
import qualified Data.Text as T
import Text.Parsec

parseSubtitles :: String -> Either ParseError [SubtitleCtx]
parseSubtitles = parse subtitles "game of thrones"

subtitles :: Parsec String () [SubtitleCtx]
subtitles = do
  subtitles <- subtitleCtx `sepBy` endOfLine
  eof
  return subtitles

subtitleCtx :: Parsec String () SubtitleCtx
subtitleCtx = do
  sequence <- read <$> many1 digit
  newline
  timingCtx <- timingCtx
  newline
  lines <- sentence `endBy` endOfLine
  return $ SubtitleCtx sequence timingCtx lines

sentence :: Parsec String () T.Text
sentence = do
  sentence <- many1 (noneOf "\n\r")
  return $ T.pack sentence

timingCtx :: Parsec String () TimingCtx
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
