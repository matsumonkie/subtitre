{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
, SentenceInfos
, WordInfos(..)
, RichSubCtx(..)
, Tag(..)
, Word
, Translations(..)
, mkTranslations
, Lemma
, or
, Level(..)
, RTranslator
, RuntimeConf(..)
, App
, LevelSet
, LevelSets(..)
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
type RichSubCtx = SubCtx [(Sentence, SentenceInfos)]

type SentenceInfos = [WordInfos]
type WordInfos = (Word, Lemma, Tag, Level)
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

data Level = Unknown
           | Easy
           | Normal
           | Hard deriving (Show, Eq, Ord)

newtype Translations = Translations (WordInfos, [Text]) deriving (Eq, Show)

type Translator = WordInfos -> IO Translations
type RTranslator a = ReaderT Translator IO a

type LevelSet = HashMap Text ()
data LevelSets = LevelSets (LevelSet, LevelSet, LevelSet)

type App a = ReaderT RuntimeConf IO a
data RuntimeConf =
  RuntimeConf { translator :: Translator
              , settings :: HashMap Text Text
              , levelSets :: LevelSets
              , levelToShow :: Level
              , subFile :: FilePath
              , dir :: FilePath
              }


mkTranslations :: WordInfos -> [Text] -> Translations
mkTranslations wi translations = Translations (wi, translations)

or :: Translations -> Translations -> Translations
or t1@(Translations(_, a)) t2 =
  if (length a > 0) then t1 else t2

p :: SentenceInfos
p = [ ("the", "", Else, Unknown)
    , ("butler", "", Else, Unknown)
    , ("looks up", "", Verb, Unknown)
    , ("his", "", Else, Unknown)
    , ("higness", "", Else, Unknown)
    ]
