{-# LANGUAGE OverloadedStrings #-}

module SpacyParser (
  parseSpacy
) where

import Common
import Prelude()

import Type
import qualified Data.Text as T
import LevelSet
import Data.Functor
import Text.Parsec
import Data.Either
import Config.App

{-
  Take a structured sentence and deserialize it
  i: [ "I -PRON- PRON\n"
     , "wanted want VERB\n"
     , "ice ice NOUN\n"
     , "creams cream NOUN"
     ]
  o: Right [
       ("I", "-PRON-", Pron, Easy)
     , ("wanted", "want", Verb, Easy)
     , ("ice", "ice", Noun, Easy)
     , ("creams", "cream", Noun, Easy)
     ]
-}
parseSpacy :: [[T.Text]] ->
              App ([Either [ParseError] [[WordInfos]]])
parseSpacy sentences =
  mapM parseSpacySentence sentences

parseSpacySentence :: [T.Text] ->
                      App (Either [ParseError] [[WordInfos]])
parseSpacySentence sentence = do
  levelSets <- asksR levelSets
  let res = parsed levelSets
  return $
    case lefts res of
      [] -> Right $ rights res
      errors -> Left errors
  where
    parsed levelSets = map (parseSentenceStructure levelSets) sentence :: [Either ParseError [WordInfos]]

parseSentenceStructure :: LevelSets -> T.Text -> Either ParseError [WordInfos]
parseSentenceStructure levelSets =
  parse (sentence levelSets) "game of thrones"

sentence :: LevelSets -> Parsec T.Text () [WordInfos]
sentence levelSets = do
  words <- many1 ((wordCtx levelSets) <* optional endOfLine)
  eof
  return words

wordCtx :: LevelSets -> Parsec T.Text () WordInfos
wordCtx levelSets = do
  orig <- word
  space
  lemma <- word
  space
  tag <- tagParser
  return (orig, lemma, tag, whichLevel (whatWord (orig, lemma, tag)) levelSets)
  where
    whatWord (orig, lemma, tag) = T.toLower $ case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> orig

tagParser :: Parsec T.Text () Tag
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

word = (T.pack <$> many1 (noneOf "\n\r ")) <?> "word"
