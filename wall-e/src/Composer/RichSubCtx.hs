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
import Control.Lens hiding (Level, to)
import Control.Monad.IO.Class
import qualified DB.WordReference as DB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T hiding (map)
import Translator.Translate
import Type
import Control.Monad.Trans.Reader

composeSubs :: [RichSubCtx] -> App Word
composeSubs subCtxs = do
  levelToShow <- asksR levelToShow
  translator <- asksR translator
  conf <- ask
  e <- liftIO $ mapConcurrently (composeSub conf) subCtxs :: App [Word]
  liftIO $ saveResponses (to $ rc conf) (responsesToSave $ tc conf)
  return $ T.intercalate "\n\n" e

saveResponses :: Language -> TVar Cache -> IO ()
saveResponses to tvCache = do
  cache <- readTVarIO tvCache
  let keys = HM.keys cache
  infoM $ "saving " <> (show $ length keys) <> " : " <> show keys
  DB.insert to $ HM.toList cache

composeSub :: Config -> RichSubCtx -> IO Word
composeSub conf (SubCtx sequence timingCtx sentences) = do
  composedSentences <- liftIO $ composeSentence conf sentences
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

composeSentence :: Config -> [(Sentence, [WordInfos])] -> IO T.Text
composeSentence conf sentencesInfos = do
  sentences <- mapConcurrently (translateSentence conf) sentencesInfos :: IO [Sentence]
  return $ T.intercalate "\n" sentences

translateSentence :: Config -> (Sentence, [WordInfos]) -> IO Sentence
translateSentence conf (sentence, sentenceInfos) = do
  translationsProcesses <- mapConcurrently (createTranslationProcess conf) sentenceInfos :: IO [Translations]
  let renderedTranslation = map (renderTranslation $ levelToShow $ rc conf) translationsProcesses
  return $ T.intercalate " " $ setCorrectSpacing (T.words sentence) renderedTranslation []

createTranslationProcess :: Config -> WordInfos -> IO (Translations)
createTranslationProcess =
  \conf wi@(word, lemma, tag, level) ->
    if shouldTranslate (levelToShow $ rc conf) wi then do
      (translator $ rc conf) conf wi
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
              "<u>" <> word <> "</u>" <> " (<i>" <> firstTranslation <> "</i>)"
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
