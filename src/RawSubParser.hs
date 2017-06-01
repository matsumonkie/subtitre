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
import LevelSet
import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Config.App
import Config.RuntimeConf

parseSubtitlesOfFile :: App [RawSubCtx]
parseSubtitlesOfFile = do
  inputFile <- askR inputFile
  result <- liftIO $ Ex.tryJust invalidArgument (readWith inputFile SIO.utf8) :: App (Either () Text)
  content <- liftIO $ either (const $ readWith inputFile SIO.latin1) return result :: App Text
  case (parseSubtitles content :: Either ParseError [RawSubCtx]) of
    Left pe -> lift $ throwE [AppError pe]
    Right rs -> return rs
  where
    readWith :: FilePath -> SIO.TextEncoding -> IO Text
    readWith inputFile encoding = do
      handle <- SIO.openFile inputFile SIO.ReadMode
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
