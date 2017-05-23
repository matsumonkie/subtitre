{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Composer.RichSubCtx (
  compose
, composeSentence
) where

import Type
import Prelude hiding (concat, words)
import Data.Text hiding (map)
import Data.Maybe
import Translator.Translate
import Data.Monoid
import Control.Lens hiding (Level)
import Control.Concurrent.Async
import Debug.Trace

compose :: Level -> [RichSubCtx] -> IO Text
compose level subCtxs =
  intercalate "\n\n" <$> mapM (composeSub level) subCtxs

composeSub :: Level -> RichSubCtx -> IO Text
composeSub level (SubCtx sequence timingCtx sentences) = do
  composedSentences <- composeSentence level sentences
  return $ intercalate "\n" [seq, composedTimingCtx, composedSentences]
  where
    seq = pack $ show sequence
    composedTimingCtx = composeTimingCtx timingCtx

composeTimingCtx :: TimingCtx -> Text
composeTimingCtx (TimingCtx btiming etiming) =
  composedTimingCtx
  where
    composedTimingCtx = intercalate " --> " [(composeTiming btiming), (composeTiming etiming)]
    composeTiming (Timing h m s ms) =
      (intercalate ":" [(intToText h), (intToText m), (intToText s)]) <> "," <> (intToText ms)

intToText :: Int -> Text
intToText i =
  pack $ if Prelude.length text < 2 then
           '0' : text
         else
           text
  where
    text = show i

composeSentence :: Level -> [(Sentence, SentenceInfos)] -> IO Text
composeSentence level sentencesInfos = do
  sentences <- mapM (translateSentence level) sentencesInfos
  return $ intercalate "\n" sentences

translateSentence :: Level -> (Sentence, SentenceInfos) -> IO Text
translateSentence level (sentence, sentenceInfos) = do
  intercalate " " <$> reorganized
  where
    reorganized = reorganizeSentence level sentence translations :: IO [Text]
    translations = map (handleTranslation level) sentenceInfos :: [Asyncable Translations]

shouldTranslate :: Level -> WordInfos -> Bool
shouldTranslate levelToShow (_, _, _, level) =
  levelToShow < level

handleTranslation :: Level -> WordInfos -> Asyncable Translations
handleTranslation levelToShow wi@(word, lemma, tag, level) =
  if shouldTranslate levelToShow wi then
    RealAsync $ (async . translate) wi
  else
    FakeAsync $ Translations (wi, [])

data Asyncable e = RealAsync (IO (Async e))
                 | FakeAsync e

instance Functor Asyncable where
  fmap f (RealAsync a) = RealAsync $ fmap (fmap f) a
  fmap f (FakeAsync a) = FakeAsync $ f a

holdOn :: Asyncable e -> IO e
holdOn (RealAsync m) = m >>= wait
holdOn (FakeAsync m) = return m

reorganizeSentence :: Level -> Sentence -> [Asyncable Translations] -> IO [Text]
reorganizeSentence level sentence translations = do
  let waitingTranslations = map (fmap $ formatWithTranslation level) translations :: [Asyncable Text]
  allTranslations <- mapM holdOn waitingTranslations :: IO [Text]
  return $ setCorrectSpacing (words sentence) allTranslations []

formatWithTranslation :: Level -> Translations -> Text
formatWithTranslation levelToShow (Translations ((word, _, _, level), translations)) =
  if levelToShow < level then
    format $ translations ^? element 0
  else
    word
  where
    format :: Maybe Text -> Text
    format translation = case translation of
      Just translation -> word <> " (" <> translation <> ")"
      Nothing -> word

setCorrectSpacing :: [Text] -> [Text] -> [Text] -> [Text]
setCorrectSpacing a@(a1:as) (b1:b2:bs) acc =
  if (Data.Text.length a1 > Data.Text.length b1) then
    setCorrectSpacing a ((b1 <> b2) : bs) acc
  else
    setCorrectSpacing as (b2:bs) (acc ++ [b1])
setCorrectSpacing (a:as) (b:bs) acc = setCorrectSpacing as bs (acc ++ [b])
setCorrectSpacing [] (b:bs) acc = setCorrectSpacing [] bs (acc ++ [b])
setCorrectSpacing _ _ acc = acc
