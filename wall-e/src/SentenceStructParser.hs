{-# LANGUAGE OverloadedStrings #-}

{-
  Take a structured sentence and deserialize it
  i: I -PRON- PRON\n
     wanted want VERB\n
     ice ice NOUN\n
     creams cream NOUN
  o: Right [
       ("I", "-PRON-", Pron, Easy)
     , ("wanted", "want", Verb, Easy)
     , ("ice", "ice", Noun, Easy)
     , ("creams", "cream", Noun, Easy)
     ]
-}

module SentenceStructParser (
  parseSentenceStructure
) where

import Type
import Data.Text
import LevelSet
import Data.Functor
import Text.Parsec
import Data.HashSet

parseSentenceStructure :: LevelSets -> Text -> Either ParseError [WordInfos]
parseSentenceStructure levelSets = parse (sentence levelSets) "game of thrones"

sentence :: LevelSets -> Parsec Text () [WordInfos]
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
      Noun -> lemma
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
