module Type (
  Sequence
, Hour
, Minute
, Second
, MSecond
, TimingCtx(..)
, Timing(..)
, Sentence
, SubtitleCtx(..)
) where

import qualified Data.Text as T

type Sequence = Int
type Sentence = T.Text
type Hour = Int
type Minute = Int
type Second = Int
type MSecond = Int

-- 00:00:26,722 --> 00:00:29,023
data TimingCtx = TimingCtx Timing Timing deriving Show
data Timing = Timing Hour Minute Second MSecond deriving Show
data SubtitleCtx = SubtitleCtx Sequence TimingCtx [Sentence] deriving Show
