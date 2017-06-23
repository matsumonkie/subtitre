{-# LANGUAGE OverloadedStrings #-}
{-
  Take an .srt file as an input and deserialize it
-}

module RawSubParser (
  parseSubtitlesOfFile
, parseSubtitles
) where

import Common
import Prelude()

import Type
import Text.Parsec.Combinator
import qualified Data.Text as T
import Text.Parsec
import qualified Data.Text.IO as TIO
import qualified System.IO as SIO
import qualified System.IO.Error as SIE
import qualified Control.Exception as Ex
import GHC.IO.Exception
import Control.Monad.Except
import Config.App
import qualified Text.HTML.TagSoup as TS
import Data.List

parseSubtitlesOfFile :: App [RawSubCtx]
parseSubtitlesOfFile = do
  inputFile <- asksR inputFile
  result <- liftIO $ Ex.tryJust invalidArgument (readWith inputFile SIO.utf8) :: App (Either () T.Text)
  content <- liftIO $ either (const $ readWith inputFile SIO.latin1) return result :: App T.Text
  case (parseSubtitles content :: Either ParseError [RawSubCtx]) of
    Left pe -> throwError [AppError pe]
    Right rs -> return rs
  where
    readWith :: FilePath -> SIO.TextEncoding -> IO T.Text
    readWith inputFile encoding = do
      handle <- SIO.openFile inputFile SIO.ReadMode
      SIO.hSetEncoding handle encoding
      TIO.hGetContents handle
    invalidArgument :: IOException -> Maybe ()
    invalidArgument IOError { ioe_type = InvalidArgument } = Just ()
    invalidArgument _ = Nothing

parseSubtitles :: T.Text -> Either ParseError [RawSubCtx]
parseSubtitles = parse subtitles "game of thrones"

subtitles :: Parsec T.Text () [RawSubCtx]
subtitles = do
  optional bom
  subtitles <- subtitleCtxP `sepBy` endOfLine
  eof
  return subtitles

subtitleCtxP :: Parsec T.Text () RawSubCtx
subtitleCtxP = do
  sequence <- sequenceP
  endOfLine
  timingCtx <- timingCtxP
  endOfLine
  lines <- sentenceWithoutTagsP `sepEndBy` endOfLine
  return $ SubCtx sequence timingCtx lines

sequenceP :: Parsec T.Text () Sequence
sequenceP =
  read <$> many1 digit <?> "RawSubParser sequence"

sentenceWithoutTagsP :: Parsec T.Text () T.Text
sentenceWithoutTagsP = do
  line <- T.pack <$> many1 (noneOf "\n\r") <?> "RawSubParser sentence"
  let withTags = TS.parseTags line :: [TS.Tag T.Text]
  return $
    Data.List.foldl' (\acc x -> acc <> (renderTextWithoutTags x)) "" withTags

renderTextWithoutTags :: TS.Tag T.Text -> T.Text
renderTextWithoutTags (TS.TagText str) = str
renderTextWithoutTags _ = ""

timingCtxP :: Parsec T.Text () TimingCtx
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
