{-# LANGUAGE OverloadedStrings #-}

module Composer.RichSubCtx (
  compose
, composeSentence
) where

import Type
import Prelude hiding (concat, length, words)
import Data.Text hiding (map)
import Data.Maybe
import Translator.Translate
import Data.Monoid
import Control.Lens
import Debug.Trace

compose :: [RichSubCtx] -> IO Text
compose subCtxs =
  intercalate "\n\n" <$> mapM composeSub subCtxs

composeSub :: RichSubCtx -> IO Text
composeSub (SubCtx sequence timingCtx sentences) = do
  composedSentences <- composeSentence sentences
  return $ intercalate "\n" [seq, composedTimingCtx, composedSentences]
  where
    seq = pack $ show sequence
    composedTimingCtx = composeTimingCtx timingCtx

composeTimingCtx :: TimingCtx -> Text
composeTimingCtx (TimingCtx btiming etiming) =
  composedTimingCtx
  where
    composedTimingCtx = intercalate " --> " [(composeTiming btiming), (composeTiming etiming)]
    composeTiming (Timing h m s ms) = intercalate ":" [(intToText h), (intToText m), (intToText s), (intToText ms)]

intToText :: Int -> Text
intToText i = pack $ show i

composeSentence :: [(Sentence, SentenceInfos)] -> IO Text
composeSentence sentencesInfos = do
  sentences <- mapM translateSentence sentencesInfos
  return $ intercalate "\n" sentences

translateSentence :: (Sentence, SentenceInfos) -> IO Text
translateSentence (sentence, sentenceInfos) = do
  intercalate " " <$> reorganized
  where
    reorganized = reorganizeSentence sentence translations :: IO [Text]
    translations = map zipWithWI sentenceInfos :: [(WordInfos, IO [Text])]
    zipWithWI :: WordInfos -> (WordInfos, IO [Text])
    zipWithWI wi = (wi, translate wi)

reorganizeSentence :: Sentence -> [(WordInfos, IO[Text])] -> IO [Text]
reorganizeSentence sentence translations = do
  translations' <- mapM getTranslations translations :: IO [(WordInfos, [Text])]
  let a = map formatWithTranslation translations' :: [Text]
  return $ setCorrectSpacing (words sentence) a []
  where
    getTranslations :: (WordInfos, IO[Text]) -> IO (WordInfos, [Text])
    getTranslations (wi, iTranslations) = do
      translations <- iTranslations
      return (wi, translations)

formatWithTranslation :: (WordInfos, [Text]) -> Text
formatWithTranslation ((word, _, _), translations) =
  case (translations ^? element 0) of
    Just translation -> word <> " (" <> translation <> ")"
    Nothing -> word

setCorrectSpacing :: [Text] -> [Text] -> [Text] -> [Text]
setCorrectSpacing a@(a1:as) (b1:b2:bs) acc =
  if (length a1 > length b1) then
    setCorrectSpacing a ((b1 <> b2) : bs) acc
  else
    setCorrectSpacing as (b2:bs) (acc ++ [b1])
setCorrectSpacing (a:as) (b:bs) acc = setCorrectSpacing as bs (acc ++ [b])
setCorrectSpacing [] (b:bs) acc = setCorrectSpacing [] bs (acc ++ [b])
setCorrectSpacing _ _ acc = acc
