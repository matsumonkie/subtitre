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
  words <- many1 (wordCtx <* optional endOfLine)
  eof
  return words

wordCtx :: Parsec Text () WordInfos
wordCtx = do
  orig <- word
  space
  lemma <- word
  space
  tag <- tagParser
  let translation = Nothing
  return (orig, lemma, tag)

tagParser :: Parsec Text () Tag
tagParser = do
  mkTag <$> word
  where
    mkTag tag = case tag of
      "VERB" -> Verb
      "ADJ" -> Adj
      _ -> Else

word = (pack <$> many1 (noneOf "\n\r ")) <?> "word"
