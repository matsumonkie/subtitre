{-
  Take a structured sentence and deserialize it
-}

{-# LANGUAGE OverloadedStrings #-}

module SentenceStructParser (
  parseSentenceStructure
) where

import Type
import Text.Parsec.Combinator
import qualified Data.Text as T
import Data.Functor
import Text.Parsec

parseSentenceStructure :: T.Text -> Either ParseError RichSentence
parseSentenceStructure = parse sentence "game of thrones"

sentence :: Parsec T.Text () RichSentence
sentence = do
  words <- wordCtx `sepBy` endOfLine
  eof
  return words

wordCtx :: Parsec T.Text () RichWord
wordCtx = do
  lemma <- word
  space
  tag <- word
  return ("", lemma, tag)
  where
    word = T.pack <$> many1 (noneOf "\n\r ")
