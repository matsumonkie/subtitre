{-# LANGUAGE OverloadedStrings #-}

{-
  Take a structured sentence and deserialize it
-}

module SentenceStructParser (
  parseSentenceStructure
) where

import Type
import Data.Text
import LevelSet
import Data.Functor
import Text.Parsec
import Prelude hiding (Word)
import Data.HashSet

parseSentenceStructure :: LevelSets -> Text -> Either ParseError SentenceInfos
parseSentenceStructure levelSets = parse (sentence levelSets) "game of thrones"

sentence :: LevelSets -> Parsec Text () SentenceInfos
sentence levelSets = do
  words <- many1 ((wordCtx levelSets) <* optional endOfLine)
  eof
  return words

wordCtx :: LevelSets -> Parsec Text () WordInfos
wordCtx levelSets = do
  orig <- word
  space
  lemma <- word
  space
  tag <- tagParser
  return (orig, lemma, tag, whichLevel (whatWord (orig, lemma, tag)) levelSets)
  where
    whatWord (orig, lemma, tag) = toLower $ case tag of
      Verb -> lemma
      _ -> orig

tagParser :: Parsec Text () Tag
tagParser = do
  mkTag <$> word
  where
    mkTag tag = case tag of
      "ADJ"   -> Adj
      "ADV"   -> Adv
      "CONJ"  -> Conj
      "NOUN"  -> Noun
      "PRON"  -> Pron
      "PUNCT" -> Punct
      "SYM"   -> Sym
      "VERB"  -> Verb
      _       -> Else

word = (pack <$> many1 (noneOf "\n\r ")) <?> "word"
