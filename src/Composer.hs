{-
  Serialize RichSub
-}

{-# LANGUAGE OverloadedStrings #-}

module Composer (
  composeSubtitles
) where

import Type
import qualified Data.Text as T

composeSubtitles :: [RawSubCtx] -> T.Text
composeSubtitles subCtxts =
  T.intercalate "\n\n" $ map composeSub subCtxts

composeSub :: RawSubCtx -> T.Text
composeSub (SubCtx sequence timingCtx sentences) =
  T.intercalate "\n" [seq, composedTimingCtx, composedSentences]
  where
    seq = T.pack $ show sequence
    composedTimingCtx = composeTimingCtx timingCtx
    composedSentences = composeSentence sentences

composeTimingCtx :: TimingCtx -> T.Text
composeTimingCtx (TimingCtx btiming etiming) =
  composedTimingCtx
  where
    composedTimingCtx = T.intercalate " --> " [(composeTiming btiming), (composeTiming etiming)]
    composeTiming (Timing h m s ms) = T.intercalate ":" [(intToText h), (intToText m), (intToText s), (intToText ms)]

intToText :: Int -> T.Text
intToText i = T.pack $ show i

composeSentence :: [Sentence] -> T.Text
composeSentence sentences =
  T.intercalate "\n" sentences
