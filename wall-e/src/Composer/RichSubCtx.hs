{-# LANGUAGE OverloadedStrings #-}

module Composer.RichSubCtx (
  composeSubs
, composeSentence
) where

import Common
import Prelude()

import Config.App
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens hiding (Level)
import Control.Monad.IO.Class
import qualified DB.WordReference as DB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T hiding (map)
import Translator.Translate
import Type

composeSubs :: [RichSubCtx] -> App Word
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
  currentNbOfOnlineRequest <- liftIO $ newTVarIO 0
  currentNbOfOfflineRequest <- liftIO $ newTVarIO 0
  let tp = TP { availableWordsInDB        = availableWordsInDB
              , translationsInCache       = translationsInCache
              , offlineWordsInProgress    = offlineWordsInProgress
              , onlineWordsInProgress     = onlineWordsInProgress
              , responsesToSave           = responsesToSave
              , currentNbOfOnlineRequest  = currentNbOfOnlineRequest
              , currentNbOfOfflineRequest = currentNbOfOfflineRequest
              }

  e <- liftIO $ mapConcurrently (composeSub tp levelToShow translator sc) subCtxs :: App [Word]
  liftIO $ saveResponses responsesToSave
  return $ T.intercalate "\n\n" e

saveResponses :: TVar Cache -> IO ()
saveResponses tvCache = do
  cache <- readTVarIO tvCache
  let keys = HM.keys cache
  infoM $ "saving " <> (show $ length keys) <> " : " <> show keys
  DB.insert $ HM.toList cache

composeSub :: TP -> Level -> Translator -> StaticConf -> RichSubCtx -> IO Word
composeSub tp levelToShow translator sc (SubCtx sequence timingCtx sentences) = do
  composedSentences <- liftIO $ composeSentence tp levelToShow translator sc sentences
  return $ T.intercalate "\n" $ subAsArray composedSentences
  where
    subAsArray :: Word -> [Word]
    subAsArray composedSentences = [ seq
                                   , composeTimingCtx timingCtx
                                   , composedSentences
                                   ]
    seq = T.pack $ show sequence

composeTimingCtx :: TimingCtx -> Word
composeTimingCtx (TimingCtx btiming etiming) =
  composedTimingCtx
  where
    composedTimingCtx = T.intercalate " --> " [(composeTiming btiming), (composeTiming etiming)]
    composeTiming (Timing h m s ms) =
      (T.intercalate ":" [(intToText h), (intToText m), (intToText s)]) <> "," <> (intToText ms)
    intToText :: Int -> Word
    intToText i =
      T.pack $ if length text < 2 then
               '0' : text
             else
               text
      where
        text = show i

composeSentence :: TP -> Level -> Translator -> StaticConf -> [(Sentence, [WordInfos])] -> IO Word
composeSentence tp levelToShow translator sc sentencesInfos = do
  sentences <- mapConcurrently (translateSentence tp levelToShow translator sc) sentencesInfos :: IO [Sentence]
  return $ T.intercalate "\n" sentences

translateSentence :: TP -> Level -> Translator -> StaticConf -> (Sentence, [WordInfos]) -> IO Sentence
translateSentence tp levelToShow translator sc (sentence, sentenceInfos) = do
  translationsProcesses <- mapConcurrently (createTranslationProcess tp levelToShow translator sc) sentenceInfos :: IO [Translations]
  let renderedTranslation = map (renderTranslation levelToShow) translationsProcesses
  return $ T.intercalate " " $ setCorrectSpacing (T.words sentence) renderedTranslation []

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
      level > levelToShow && tag `elem` [Verb, Noun, Adj, Propn, Adv]

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
    format :: Maybe Word -> Word
    format translation = case translation of
      Just translation ->
        if word /= translation then
          case (T.splitOn ", " translation) of
            (firstTranslation : otherTranslations) ->
              word <> " (" <> firstTranslation <> ")"
            _ -> word
        else
          word
      Nothing -> word

setCorrectSpacing :: [Word] -> [Word] -> [Word] -> [Word]
setCorrectSpacing a@(a1:as) (b1:b2:bs) acc =
  if (T.length a1 > T.length b1) then
    setCorrectSpacing a ((b1 <> b2) : bs) acc
  else
    setCorrectSpacing as (b2:bs) (acc ++ [b1])
setCorrectSpacing (a:as) (b:bs) acc = setCorrectSpacing as bs (acc ++ [b])
setCorrectSpacing [] (b:bs) acc = setCorrectSpacing [] bs (acc ++ [b])
setCorrectSpacing _ _ acc = acc
