{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Composer.RichSubCtx (
  composeSubs
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
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Config.App

composeSubs :: [RichSubCtx] -> App Text
composeSubs subCtxs = do
  e <- mapM composeSub subCtxs :: App [Text]
  return $ intercalate "\n\n" e

composeSub :: RichSubCtx -> App Text
composeSub (SubCtx sequence timingCtx sentences) = do
  composedSentences <- composeSentence sentences :: App Text
  return $ intercalate "\n" $ subAsArray composedSentences
  where
    subAsArray :: Text -> [Text]
    subAsArray composedSentences = [ seq
                                   , composeTimingCtx timingCtx
                                   , composedSentences
                                   ]
    seq = pack $ show sequence

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

composeSentence :: [(Sentence, SentenceInfos)] -> App Text
composeSentence sentencesInfos = do
  sentences <- mapM translateSentence sentencesInfos :: App [Text]
  return $ intercalate "\n" sentences

translateSentence :: (Sentence, SentenceInfos) -> App Text
translateSentence (sentence, sentenceInfos) = do
  text <- reorganized :: App [Text]
  return $ intercalate " " text
  where
    translations = mapM (handleTranslation) sentenceInfos :: App [(Asyncable Translations)]
    reorganized :: App [Text]
    reorganized = do
      tr' <- translations
      reorganizeSentence sentence tr' :: App [Text]

handleTranslation :: WordInfos -> App (Asyncable Translations)
handleTranslation wi@(word, lemma, tag, level) = do
  levelToShow <- asksR levelToShow
  translator <- asksR translator
  sc <- askS
  return $ if shouldTranslate levelToShow level then
    RealAsync $ (async . (translator sc)) wi
  else
    FakeAsync $ mkTranslations wi []
  where
    shouldTranslate levelToShow level = levelToShow < level

reorganizeSentence :: Sentence -> [Asyncable Translations] -> App [Text]
reorganizeSentence sentence translations = do
  levelToShow <- asksR levelToShow
  let waitingTranslations = map (fmap $ formatWithTranslation levelToShow) translations :: [Asyncable Text]
  allTranslations <- liftIO $ mapM holdOn waitingTranslations
  return $ setCorrectSpacing (words sentence) allTranslations []

formatWithTranslation :: Level -> Translations -> Text
formatWithTranslation levelToShow (Translations' ((word, _, _, level), translations)) = do
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

data Asyncable e = RealAsync (IO (Async e))
                 | FakeAsync e

instance Functor Asyncable where
  fmap f (RealAsync a) = RealAsync $ fmap (fmap f) a
  fmap f (FakeAsync a) = FakeAsync $ f a

holdOn :: Asyncable e -> IO e
holdOn (RealAsync m) = m >>= wait
holdOn (FakeAsync m) = return m
