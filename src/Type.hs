{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
, SentenceInfos
, WordInfos(..)
, RichSubCtx(..)
, Tag(..)
, Translation
) where

import Data.Text
import Prelude hiding (Word)
import Data.Aeson
import GHC.Generics
import GHC.Exts hiding (Word)
import qualified Data.HashMap.Strict as HM
import Data.Monoid

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
type WordInfos = (Word, Lemma, Tag)
type Word = Text
type Lemma = Text
data Tag = Verb | Adj | Else deriving (Show, Eq)
type Translation = (WordInfos, Maybe Text)

p :: SentenceInfos
p = [ ("the", "", Else)
    , ("butler", "", Else)
    , ("looks up", "", Verb)
    , ("his", "", Else)
    , ("higness", "", Else)
    ]
