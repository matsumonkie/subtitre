{-# LANGUAGE OverloadedStrings #-}
{-
  Take an .srt file as an input and deserialize it
-}

module RawSubParser (
  parseSubtitlesOfFile
, parseSubtitles
) where

import Type
import Text.Parsec.Combinator
import Data.Text
import Text.Parsec
import Data.Functor.Identity
import Data.Monoid
import Control.Monad
import Data.Text.IO
import qualified System.IO as SIO
import qualified System.IO.Error as SIE
import qualified Control.Exception as Ex
import GHC.IO.Exception


parseSubtitlesOfFile :: FilePath -> IO (Either ParseError [RawSubCtx])
parseSubtitlesOfFile file = do
  result <- Ex.tryJust invalidArgument (readWith SIO.utf8) :: IO (Either () Text)
  content <- either (const $ readWith SIO.latin1) return result :: IO Text
  return $ parseSubtitles content
  where
    readWith :: SIO.TextEncoding -> IO Text
    readWith encoding = do
      handle <- SIO.openFile file SIO.ReadMode
      SIO.hSetEncoding handle encoding
      hGetContents handle
    invalidArgument :: IOException -> Maybe ()
    invalidArgument IOError { ioe_type = InvalidArgument } = Just ()
    invalidArgument _ = Nothing

parseSubtitles :: Text -> Either ParseError [RawSubCtx]
parseSubtitles = parse subtitles "game of thrones"

subtitles :: Parsec Text () [RawSubCtx]
subtitles = do
  optional bom
  subtitles <- subtitleCtxP `sepBy` endOfLine
  eof
  return subtitles

subtitleCtxP :: Parsec Text () RawSubCtx
subtitleCtxP = do
  sequence <- sequenceP
  endOfLine
  timingCtx <- timingCtxP
  endOfLine
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
  bsec <- readSeconds
  comma
  bmsec <- readMSeconds
  separator
  ehour <- twoDigits
  colon
  emin <- twoDigits
  colon
  esec <- readSeconds
  comma
  emsec <- readMSeconds
  return $ TimingCtx (Timing bhour bmin bsec bmsec) (Timing ehour emin esec emsec)
  where
    readSeconds = try twoDigits <|> oneDigit
    readMSeconds = try threeDigits <|> twoDigits
    oneDigit = read <$> Text.Parsec.count 1 digit
    twoDigits = read <$> Text.Parsec.count 2 digit
    threeDigits = read <$> Text.Parsec.count 3 digit
    colon = char ':'
    comma = char ','
    separator = (string " --> " <?> "RawSubParser timingCtx separator")

bom = char '\65279'
