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
type RichSubCtx' = (RawSubCtx, SentenceInfos)

-- (SubCtx Sequence TimingCtx [Sentence], [SentenceInfos])


type SentenceInfos = [WordInfos]
type WordInfos = (Word, Lemma, Tag)
type Word = T.Text
type Lemma = T.Text
data Tag = Verb | Adj | Else deriving (Show, Eq)

p :: SentenceInfos
p = [ ("the", "", Else)
    , ("butler", "", Else)
    , ("looks up", "", Verb)
    , ("his", "", Else)
    , ("higness", "", Else)
    ]

instance ToJSON WordInfos where
  toJSON (word, lemma, tag) =
    object [ "word"  .= word
           , "type"  .= lemma
           ]

instance ToJSON TimingCtx where
  toJSON (TimingCtx (Timing bh bm bs bms) (Timing h m s ms)) =
    object [ "h" .= h
           ]

instance ToJSON RichSubCtx where
  toJSON (SubCtx sequence timingCtx sentences) =
    object [ "sequence" .= sequence
           , "timingCtx" .= timingCtx
           , "sentences" .= sentences
           ]
