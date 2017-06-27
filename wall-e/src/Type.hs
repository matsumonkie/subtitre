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
, Translations(..)
, Translations'(..)
, mkTranslations
, Lemma
, Level(..)
, LevelSet
, LevelSets(..)
, Cache
, TakenCare
, Language
) where

import Common
import Prelude()

import Control.Concurrent.STM
import Control.DeepSeq
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Text as T
import GHC.Generics (Generic)

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

type Language = String
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

newtype Translations' a =
  Translations' (WordInfos, [a]) deriving (Eq, Show, Functor)

type Translations = Translations' T.Text

type WordInfos = (Word, Lemma, Tag, Level)

data Level = Easy
           | Normal
           | Hard
           | Unknown
           deriving (Read, Show, Eq, Ord, Generic, NFData)

type LevelSet = HS.HashSet T.Text
data LevelSets = LevelSets (LevelSet, LevelSet, LevelSet) deriving (Generic, NFData, Show)

mkTranslations :: WordInfos -> [Word] -> Translations
mkTranslations wi translations = Translations' (wi, translations)

type Cache = HM.HashMap T.Text (Maybe Value)
type SetKey = TVar [T.Text]
type TakenCare = [T.Text]
