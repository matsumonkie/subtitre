{-# LANGUAGE OverloadedStrings #-}

module Composer.RichSubCtx (
  composeSubs
, composeSentence
) where

import Type
import Prelude hiding (concat, words, Word)
import Data.Text hiding (map)
import Data.Maybe
import Translator.Translate
import Data.Monoid
import Control.Lens hiding (Level)
import Control.Concurrent.Async
import Debug.Trace
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Config.App
import Text.Pretty.Simple (pPrint, pString)
import qualified Logger as L

composeSubs :: [RichSubCtx] -> App Text
composeSubs subCtxs = do
  levelToShow <- asksR levelToShow
  translator <- asksR translator
  sc <- askS
  e <- liftIO $ mapConcurrently (composeSub levelToShow translator sc) subCtxs :: App [Text]
  return $ intercalate "\n\n" e

composeSub :: Level -> Translator -> StaticConf -> RichSubCtx -> IO Text
composeSub levelToShow translator sc (SubCtx sequence timingCtx sentences) = do
  composedSentences <- liftIO $ composeSentence levelToShow translator sc sentences
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

composeSentence :: Level -> Translator -> StaticConf -> [(Sentence, [WordInfos])] -> IO Text
composeSentence levelToShow translator sc sentencesInfos = do
  sentences <- mapConcurrently (translateSentence levelToShow translator sc) sentencesInfos :: IO [Sentence]
  return $ intercalate "\n" sentences

translateSentence :: Level -> Translator -> StaticConf -> (Sentence, [WordInfos]) -> IO Sentence
translateSentence levelToShow translator sc (sentence, sentenceInfos) = do
  translationsProcesses <- mapConcurrently (createTranslationProcess levelToShow translator sc) sentenceInfos :: IO [Translations]
  let renderedTranslation = map (renderTranslation levelToShow) translationsProcesses
  return $ intercalate " " $ setCorrectSpacing (words sentence) renderedTranslation []

createTranslationProcess :: Level -> Translator -> StaticConf -> WordInfos -> IO (Translations)
createTranslationProcess =
  \levelToShow translator sc wi@(word, lemma, tag, level) ->
    if shouldTranslate levelToShow wi then do
      translator sc wi
    else
      return $ mkTranslations wi []
  where
    shouldTranslate :: Level -> WordInfos -> Bool
    shouldTranslate levelToShow wi@(word, lemma, tag, level) = do
      level > levelToShow && tag `elem` [Verb, Noun, Adj, Propn]

{-
  i: (("whisper", "whisper", Verb, Easy), ["murmurer"])
  o: "whisper (murmurer)"
-}
renderTranslation :: Level -> Translations -> Word
renderTranslation levelToShow (Translations' ((word, _, _, level), translations)) = do
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
