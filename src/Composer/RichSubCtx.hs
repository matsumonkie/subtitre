{-
  Serialize RichSub
-}

{-# LANGUAGE OverloadedStrings #-}

module Composer.RichSubCtx (
  compose
) where

import Type
import Prelude hiding (concat, length, words)
import Data.Text hiding (map)
import Data.Maybe
import Translator.Word
import Data.Monoid

compose :: [RichSubCtx] -> Text
compose subCtxs =
  intercalate "\n\n" $ map composeSub subCtxs

composeSub :: RichSubCtx -> Text
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

composeSentence :: [(Sentence, SentenceInfos)] -> Text
composeSentence sentencesInfos =
  intercalate "\n" $ map translateSentence sentencesInfos

translateSentence :: (Sentence, SentenceInfos) -> Text
translateSentence (sentence, sentencesInfos) =
  intercalate " " reorganized
  where
    reorganized = reorganizeSentence translations sentence
    translations = map translate sentencesInfos

reorganizeSentence :: [Translation] -> Sentence -> [Text]
reorganizeSentence translations sentence =
  map format withCorrectSpacing
  where
    format :: Translation -> Text
    format ((word, _, _), mTranslation) =
      case mTranslation of
        Just translation -> word <> " (" <> translation <> ")"
        Nothing -> word
    withCorrectSpacing = setCorrectSpacing (words sentence) translations []

setCorrectSpacing :: [Text] -> [Translation] -> [Translation] -> [Translation]
setCorrectSpacing a@(a1:as) (b1:b2:bs) acc =
  if (length a1 > translationLength b1) then
    setCorrectSpacing a ((mergeTranslations b1 b2) : bs) acc
  else
    setCorrectSpacing as (b2:bs) (acc ++ [b1])
setCorrectSpacing (a:as) (b:bs) acc = setCorrectSpacing as bs (acc ++ [b])
setCorrectSpacing _ _ acc = acc

translationLength :: Translation -> Int
translationLength ((word, _, _), _) = length word

mergeTranslations :: Translation -> Translation -> Translation
mergeTranslations t1@((word1, lemma1, tag1), mTr1) t2@((word2, lemma2, tag2), mTr2) =
  ((word1 <> word2, lemma1, tag1), mTr1 <> mTr2)
