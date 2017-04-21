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
) where

import qualified Data.Text as T
import Prelude hiding (Word)
import Data.Aeson
import GHC.Generics

type Sequence = Int
type Sentence = T.Text
type Hour = Int
type Minute = Int
type Second = Int
type MSecond = Int

-- 00:00:26,722 --> 00:00:29,023
data TimingCtx = TimingCtx Timing Timing deriving (Show, Eq)
data Timing = Timing Hour Minute Second MSecond deriving (Show, Eq)
data SubCtx a = SubCtx Sequence TimingCtx a deriving (Show, Eq)
type RawSubCtx  = SubCtx [Sentence]
type RichSubCtx = SubCtx [(Sentence, SentenceInfos)]

type SentenceInfos = [WordInfos]
type WordInfos = (Word, Lemma, Tag)
type Word = T.Text
type Lemma = T.Text
type Tag = T.Text

p :: SentenceInfos
p = [ ("the", "", "")
    , ("butler", "", "")
    , ("looks up", "", "")
    , ("his", "", "")
    , ("higness", "", "")
    ]

instance ToJSON WordInfos where
  toJSON (word, lemma, tag) =
    object [ "word"  .= word
           , "type"  .= lemma
           ]
