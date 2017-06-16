{-
  Serialize RawSub
-}

{-# LANGUAGE OverloadedStrings #-}

module Composer.RawSubCtx (
  compose
) where

import Type
import Data.Text hiding (map)

compose :: [RawSubCtx] -> Text
compose subCtxts =
  intercalate "\n\n" $ map composeSub subCtxts

composeSub :: RawSubCtx -> Text
composeSub (SubCtx sequence timingCtx sentences) =
  intercalate "\n" [seq, composedTimingCtx, composedSentences]
  where
    seq = pack $ show sequence
    composedTimingCtx = composeTimingCtx timingCtx
    composedSentences = composeSentence sentences

composeTimingCtx :: TimingCtx -> Text
composeTimingCtx (TimingCtx btiming etiming) =
  composedTimingCtx
  where
    composedTimingCtx = intercalate " --> " [(composeTiming btiming), (composeTiming etiming)]
    composeTiming (Timing h m s ms) = intercalate ":" [(intToText h), (intToText m), (intToText s), (intToText ms)]

intToText :: Int -> Text
intToText i = pack $ show i

composeSentence :: [Sentence] -> Text
composeSentence sentences =
  intercalate "\n" sentences
