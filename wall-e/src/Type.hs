{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DeriveFunctor #-}

module Type (
  Sequence
, Hour
, Minute
, Second
, MSecond
, TimingCtx(..)
, Timing(..)
, Sentence
, SubCtx(..)
, RawSubCtx
, WordInfos(..)
, RichSubCtx(..)
, Tag(..)
, Word
, Translations
, Lemma
, Level(..)
, TextSet
, LevelSets(..)
, Cache
, TakenCare
, Language
, Json
) where

import Common
import Prelude()

import Control.Concurrent.STM
import Control.DeepSeq
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import Text.Read
import Data.Char
import Text.ParserCombinators.ReadP hiding (choice)
import Text.ParserCombinators.ReadPrec hiding (choice)

type Sequence = Int
type Sentence = T.Text
type Hour = Int
type Minute = Int
type Second = Int
type MSecond = Int

data TimingCtx = TimingCtx Timing Timing deriving (Show, Eq)
data Timing = Timing Hour Minute Second MSecond deriving (Show, Eq)
data SubCtx a = SubCtx Sequence TimingCtx a deriving (Show, Eq)
type RawSubCtx  = SubCtx [Sentence]
type RichSubCtx = SubCtx [(Sentence, [WordInfos])]

type Json = Value
type Language = T.Text
type Word = T.Text
type Lemma = T.Text
data Tag = Adj
         | Adv
         | Conj
         | Noun
         | Num
         | Pron
         | Propn
         | Punct
         | Sym
         | Verb
         | Else
         deriving (Show, Eq)

type Translations = (Word, WordInfos, [Word])

type WordInfos = (Word, Lemma, Tag, Level)

data Level = Easy
           | Normal
           | Hard
           | Unknown
           deriving (Show, Eq, Ord)

instance Read Level where
  readPrec =
    choice $ strValMap [ ("easy", Easy)
                       , ("normal", Normal)
                       , ("hard", Hard)
                       ]
    where
      strValMap :: [(String, a)] -> [ReadPrec a]
      strValMap = map (\(x, y) -> lift $ string x >> return y)

type Set = HS.HashSet
type TextSet = Set T.Text
data LevelSets = LevelSets (TextSet, TextSet, TextSet) deriving (Show)

type Cache = HM.HashMap Word (Maybe Value)
type SetKey = TVar [T.Text]
type TakenCare = [T.Text]
