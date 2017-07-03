{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Composer.RichSubCtx (
  compose
, composeSentence
) where

import Common
import Prelude()

import Config.App
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Lens hiding (Level, to)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified DB.WordReference as DB
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T hiding (map)
import Deserializer.WordReference
import Network.Wreq
import qualified Translator.Strategy.WordReference as WRStrategy
import Translator.Translate
import Type

compose :: Cache -> [RichSubCtx] -> App T.Text
compose cache subCtxs = do
  levelToShow <- asksR levelToShow
  conf <- ask
  let composed = map (composeSub cache levelToShow) subCtxs :: [Word]
  liftIO $ saveResponses conf (toLang $ rc conf) cache
  return $ T.intercalate "\n\n" composed

composeSub :: Cache -> Level -> RichSubCtx -> T.Text
composeSub cache level (SubCtx sequence timingCtx sentences) = do
  let composedSentences = composeSentence cache level sentences
  T.intercalate "\n" $ subAsArray composedSentences
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

composeSentence :: Cache -> Level -> [(Sentence, [WordInfos])] -> T.Text
composeSentence cache levelToShow sentencesInfos = do
  T.intercalate "\n" $ map (translateSentence cache levelToShow) sentencesInfos

translateSentence :: Cache -> Level -> (Sentence, [WordInfos]) -> Sentence
translateSentence cache levelToShow (sentence, sentenceInfos) = do
  let keysToWis = map toKeyable sentenceInfos :: [(Word, WordInfos)]
  T.intercalate "\n" $ map (renderWord cache levelToShow) keysToWis

renderWord :: Cache -> Level -> (Word, WordInfos) -> Word
renderWord cache levelToShow (key, wi@(word, lemma, tag, level)) =
  if shouldBeTranslated levelToShow wi then
    let (_, _, trs) = toTranslations cache (key, wi)
        tr = trs ^? element 0
    in
      maybe word (format word) tr
  else
    word
  where
    format :: Word -> Word -> Word
    format word translation =
      if word /= translation then
        case (T.splitOn ", " translation) of
          (firstTranslation : otherTranslations) ->
            "<u>" <> word <> "</u>" <> " (<i>" <> firstTranslation <> "</i>)"
          _ -> word
      else
        word

toTranslations :: Cache -> (Word, WordInfos) -> Translations
toTranslations cache (key, wi@(word, lemma, tag, level)) =
  let
    translations = case key `HM.lookup` cache of
      Nothing -> []
      Just (mTrs) -> translationsFromValue mTrs wi
  in
    (key, wi, translations)
  where
    translationsFromValue :: Maybe Value -> WordInfos -> [Word]
    translationsFromValue mValue (_, _, tag, _) =
      maybe [] (translationsBasedOnTag tag) mValue

translationsBasedOnTag :: Tag -> Value -> [Word]
translationsBasedOnTag tag value = do
  case (fromJSON value :: Result WRResponse) of
    Error _ -> []
    Success wrResponse -> do
      let translations = allTranslations wrResponse
      let correctTrs = filter (\x -> tag == tPos x) translations
      if null correctTrs then
        map tTerm translations
      else
        map tTerm correctTrs

saveResponses :: Config -> Language -> Cache -> IO ()
saveResponses conf toLang cache = do
  let keys = HM.keys cache
  infoM $ "saving " <> (show $ length keys) <> " : " <> show keys
  DB.insert conf "wordreference" toLang $ HM.toList cache

setCorrectSpacing :: [Word] -> [Word] -> [Word] -> [Word]
setCorrectSpacing a@(a1:as) (b1:b2:bs) acc =
  if (T.length a1 > T.length b1) then
    setCorrectSpacing a ((b1 <> b2) : bs) acc
  else
    setCorrectSpacing as (b2:bs) (acc ++ [b1])
setCorrectSpacing (a:as) (b:bs) acc = setCorrectSpacing as bs (acc ++ [b])
setCorrectSpacing [] (b:bs) acc = setCorrectSpacing [] bs (acc ++ [b])
setCorrectSpacing _ _ acc = acc
