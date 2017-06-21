{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
, TP(..)
, Cache
, TakenCare
) where

import Data.Text hiding (length)
import Prelude hiding (Word, or)
import Data.Aeson
import GHC.Generics
import GHC.Exts hiding (Word)
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Functor.Identity
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader (ReaderT, runReaderT)
import Data.HashMap.Strict
import Control.Monad.Trans.Except
import Text.Parsec
import Data.HashSet
import Control.DeepSeq
import GHC.Generics (Generic)
import Control.Applicative
import Control.Concurrent.STM
import Debug.Trace

type Sequence = Int
type Sentence = Text
type Hour = Int
type Minute = Int
type Second = Int
type MSecond = Int

data TimingCtx = TimingCtx Timing Timing deriving (Show, Eq)
data Timing = Timing Hour Minute Second MSecond deriving (Show, Eq)
data SubCtx a = SubCtx Sequence TimingCtx a deriving (Show, Eq)
type RawSubCtx  = SubCtx [Sentence]
type RichSubCtx = SubCtx [(Sentence, [WordInfos])]

type Word = Text
type Lemma = Text
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

type Translations = Translations' Text

type WordInfos = (Word, Lemma, Tag, Level)

data Level = Easy
           | Normal
           | Hard
           | Unknown
           deriving (Show, Eq, Ord, Generic, NFData)

type LevelSet = HashSet Text
data LevelSets = LevelSets (LevelSet, LevelSet, LevelSet) deriving (Generic, NFData)

mkTranslations :: WordInfos -> [Text] -> Translations
mkTranslations wi translations = Translations' (wi, translations)

type Cache = HashMap Text (Maybe Value)
type SetKey = TVar [Text]
type TakenCare = [Text]

data TP = TP { availableWordsInDB :: TVar [Text]
             , translationsInCache :: TVar Cache
             , onlineWordsInProgress :: TVar TakenCare
             , offlineWordsInProgress :: TVar TakenCare
             , responsesToSave :: TVar Cache
             }
