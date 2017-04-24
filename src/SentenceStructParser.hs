{-# LANGUAGE OverloadedStrings #-}

{-
  Take a structured sentence and deserialize it
-}

module SentenceStructParser (
  parseSentenceStructure
) where

import Type
import qualified Data.Text as T
import Data.Functor
import Text.Parsec

parseSentenceStructure :: T.Text -> Either ParseError SentenceInfos
parseSentenceStructure = parse sentence "game of thrones"

sentence :: Parsec T.Text () SentenceInfos
sentence = do
  words <- many1 (wordCtx <* optional endOfLine) <?> "A"
  eof <?> "C"
  return words

wordCtx :: Parsec T.Text () WordInfos
wordCtx = do
  lemma <- word <?> "D"
  space <?> "E"
  tag <- word <?> "F"
  return ("", lemma, tag)
  where
    word = (T.pack <$> many1 (noneOf "\n\r ")) <?> "G"
