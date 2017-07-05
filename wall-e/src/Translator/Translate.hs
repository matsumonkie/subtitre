{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Translator.Translate (
  translate
, toKeyable
, shouldBeTranslated
) where

import Common
import Prelude()

import Data.List
import Control.Monad.Trans.Reader
import Control.Concurrent.Async
import Config.App
import Control.Concurrent.STM
import Control.Exception
import Control.Lens hiding (to, Level)
import Control.Monad
import qualified DB.WordReference as DB
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import Deserializer.WordReference
import Network.Wreq
import qualified Translator.Strategy.WordReference as WRStrategy
import Type
import Control.Monad.IO.Class

translate :: [RichSubCtx] -> App Cache
translate richSubCtxs = do
  conf <- ask
  levelToShow <- asksR levelToShow
  let words = wordsToTranslate levelToShow richSubCtxs
  offlineCache <- liftIO $ cacheFromDB conf words
  liftIO $
    infoM $ (show $ length words) <> " words to translate " <>
    "with " <> (show $ HM.size offlineCache) <> " words already cached "
  let notCached = getNotCached offlineCache words

  onlineResponses <- liftIO $ mapConcurrently (translateOnline conf) notCached
  liftIO $ saveResponses conf (toLang $ rc conf) onlineResponses
  let onlineCache = HM.fromList onlineResponses
  return $ offlineCache `HM.union` onlineCache
  where
    getNotCached offlineCache words =
      filter (\(k, _) -> not $ (T.toLower k) `HM.member` offlineCache) words
    saveResponses :: Config -> Language -> [(Word, Maybe Json)] -> IO ()
    saveResponses conf toLang responses = do
      infoM $ "saving " <> (show $ length responses) <> " new words"
      DB.insertAll conf "wordreference" toLang responses

wordsToTranslate :: Level -> [RichSubCtx] -> [(Word, WordInfos)]
wordsToTranslate levelToShow richSubCtxs =
  uniq
    $ map toKeyable
    $ filter (shouldBeTranslated levelToShow)
    $ richSubCtxs >>= onlyWordsInfo

onlyWordsInfo :: RichSubCtx -> [WordInfos]
onlyWordsInfo (SubCtx _ _ sentences) =
  sentences >>= snd

uniq :: [(Word, WordInfos)] -> [(Word, WordInfos)]
uniq wis =
  foldl' f [] wis
  where
    sameWord :: (Word, WordInfos) -> (Word, WordInfos) -> Bool
    sameWord (key1, _) (key2, _) = key1 == key2
    f :: [(Word, WordInfos)] -> (Word, WordInfos) -> [(Word, WordInfos)]
    f acc' wi =
      if isJust $ find (sameWord wi) acc' then
        acc'
      else
        wi : acc'

toKeyable :: WordInfos -> (Word, WordInfos)
toKeyable wi =
  (whatToTranslate wi, wi)

shouldBeTranslated :: Level -> WordInfos -> Bool
shouldBeTranslated levelToShow wi@(_, _, tag, level) =
  level > levelToShow && tag `elem` [Verb, Noun, Adj, Propn, Adv]

whatToTranslate :: WordInfos -> Word
whatToTranslate (word, lemma, tag, _) = case tag of
  Verb -> lemma
  Noun -> lemma
  _ -> word

cacheFromDB :: Config -> [(Word, WordInfos)] -> IO Cache
cacheFromDB conf keysToWis = do
  HM.fromList <$> DB.selectAll conf (map fst keysToWis)

translateOnline :: Config -> (Word, WordInfos) -> IO (Word, Maybe Json)
translateOnline conf@(Config {rc, sc, tc})  (key, wi) = do
  let onlineRequest = currentNbOfOnlineRequest tc
  atomically $ do
    waitForPool onlineRequest $ wordReferencePool sc
    acquireRequestPool onlineRequest
  response <- WRStrategy.fetch conf key
  atomically $ releaseRequestPool onlineRequest

  let json = response >>= body >>= decode >>= filterValidResponse :: Maybe Json
  return $ (key, json)
  where
    body :: Response BSL.ByteString -> Maybe BSL.ByteString
    body response = do
      return $ response ^. responseBody

filterValidResponse :: Json -> Maybe Json
filterValidResponse json =
  case (fromJSON json :: Result WRResponse) of
    Success _ -> Just json
    _ -> Nothing

waitForPool :: TVar Int -> Int -> STM ()
waitForPool count max = do
  nbRequest <- readTVar count
  if nbRequest >= max then
    retry
  else
    return ()

addRequestPool :: TVar Int -> Int -> STM ()
addRequestPool count n = do
  nbRequest <- readTVar count
  writeTVar count $ nbRequest + n

releaseRequestPool count = addRequestPool count (-1)
acquireRequestPool count = addRequestPool count 1
