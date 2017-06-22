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
import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import qualified DB.WordReference as DB
import Data.Aeson

composeSubs :: [RichSubCtx] -> App Text
composeSubs subCtxs = do
  levelToShow <- asksR levelToShow
  translator <- asksR translator
  sc <- askS
  available <- liftIO $ DB.available sc
  availableWordsInDB <- liftIO $ newTVarIO available
  translationsInCache <- liftIO $ newTVarIO $ HM.fromList([])
  offlineWordsInProgress <- liftIO $ newTVarIO []
  onlineWordsInProgress <- liftIO $ newTVarIO []
  responsesToSave <- liftIO $ newTVarIO $ HM.fromList([])
  let tp = TP { availableWordsInDB     = availableWordsInDB
              , translationsInCache    = translationsInCache
              , offlineWordsInProgress = offlineWordsInProgress
              , onlineWordsInProgress  = onlineWordsInProgress
              , responsesToSave        = responsesToSave
              }

  e <- liftIO $ mapConcurrently (composeSub tp levelToShow translator sc) subCtxs :: App [Text]
  liftIO $ saveResponses responsesToSave
  return $ intercalate "\n\n" e

saveResponses :: TVar Cache -> IO ()
saveResponses tvCache = do
  cache <- readTVarIO tvCache
  DB.insert $ HM.toList cache

composeSub :: TP -> Level -> Translator -> StaticConf -> RichSubCtx -> IO Text
composeSub tp levelToShow translator sc (SubCtx sequence timingCtx sentences) = do
  composedSentences <- liftIO $ composeSentence tp levelToShow translator sc sentences
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

composeSentence :: TP -> Level -> Translator -> StaticConf -> [(Sentence, [WordInfos])] -> IO Text
composeSentence tp levelToShow translator sc sentencesInfos = do
  sentences <- mapConcurrently (translateSentence tp levelToShow translator sc) sentencesInfos :: IO [Sentence]
  return $ intercalate "\n" sentences

translateSentence :: TP -> Level -> Translator -> StaticConf -> (Sentence, [WordInfos]) -> IO Sentence
translateSentence tp levelToShow translator sc (sentence, sentenceInfos) = do
  translationsProcesses <- mapConcurrently (createTranslationProcess tp levelToShow translator sc) sentenceInfos :: IO [Translations]
  let renderedTranslation = map (renderTranslation levelToShow) translationsProcesses
  return $ intercalate " " $ setCorrectSpacing (words sentence) renderedTranslation []

createTranslationProcess :: TP -> Level -> Translator -> StaticConf -> WordInfos -> IO (Translations)
createTranslationProcess tp =
  \levelToShow translator sc wi@(word, lemma, tag, level) ->
    if shouldTranslate levelToShow wi then do
      translator tp sc wi
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
