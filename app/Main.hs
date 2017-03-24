{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import qualified Data.Text as T hiding (map, head)
import qualified Data.Text.IO as IO
import Text.Pretty.Simple (pPrint)

import qualified Text.Parsec as Parsec

subtitleFile = "test.srt"

type Sequence = Int
data Timing = Timing T.Text deriving Show
type Sentence = T.Text

data SubtitleCtx = SubtitleCtx Sequence Timing [Sentence] deriving Show

subParser :: Parsec.Parsec String () SubtitleCtx
subParser = do
  seq <- read <$> Parsec.many1 Parsec.digit
  Parsec.newline
  timing <- timingParser
  Parsec.newline
  sentences <- Parsec.manyTill sentenceParser Parsec.newline
  return $ SubtitleCtx seq (Timing timing) (map T.pack sentences)

sentenceParser :: Parsec.Parsec String () String
sentenceParser = do
  sentence <- Parsec.manyTill Parsec.anyChar Parsec.newline
  return sentence

-- 00:00:26,722 --> 00:00:29,023
timingParser :: Parsec.Parsec String () T.Text
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
  return $ T.pack (bhour ++ bmin ++ bsec ++ bmsec ++ ":" ++ ehour ++ emin ++ esec ++ emsec)
  where
    twoDigits = Parsec.count 2 Parsec.digit
    threeDigits = Parsec.count 3 Parsec.digit
    colon = Parsec.char ':'
    comma = Parsec.char ','
    separator = Parsec.string " --> "


main :: IO ()
main = do
  fileContent <- readFile subtitleFile
  case parse fileContent of
    Right subCtx@(SubtitleCtx a b c) -> pPrint subCtx
    _ -> pPrint ""
  where
    parse = Parsec.parse subParser ""

--main = undefined
