{-# LANGUAGE OverloadedStrings #-}

{-
  Take a structured sentence and deserialize it
-}

module SentenceStructParser (
  parseSentenceStructure
) where

import Type
import Data.Text
import Data.Functor
import Text.Parsec

parseSentenceStructure :: Text -> Either ParseError SentenceInfos
parseSentenceStructure = parse sentence "game of thrones"

sentence :: Parsec Text () SentenceInfos
sentence = do
  words <- many1 (wordCtx <* optional endOfLine) <?> "A"
  eof <?> "C"
  return words

wordCtx :: Parsec Text () WordInfos
wordCtx = do
  lemma <- word <?> "D"
  space <?> "E"
  tag <- word <?> "F"
  return ("", lemma, tag)
  where
    word = (pack <$> many1 (noneOf "\n\r ")) <?> "G"
