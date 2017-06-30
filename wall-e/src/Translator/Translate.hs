{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Translator.Translate (
  translate
) where

import Common
import Prelude()

import Config.App
import Control.Concurrent.STM
import Control.Exception
import Control.Lens hiding (to)
import Control.Monad
import qualified DB.WordReference as DB
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import qualified Data.Text as T
import Deserializer.WordReference
import Network.Wreq
import Type
import qualified Translator.Strategy.WordReference as WRStrategy

translate :: Config -> WordInfos -> IO Translations
translate conf wi@(_, _, tag, _) = do
  words <- translate' conf wi $ toTranslate wi
  return $ mkTranslations wi words
  where
    toTranslate :: WordInfos -> Word
    toTranslate (word, lemma, tag, _) = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

translate' :: Config -> WordInfos -> Word -> IO [Word]
translate' conf@(Config {rc, sc, tc}) wi key = do
  availableWordsInDB' <- readTVarIO $ availableWordsInDB tc
  if key `elem` availableWordsInDB' then
    offline conf wi key
  else
    online conf wi key

offline :: Config -> WordInfos -> Word -> IO [Word]
offline conf@(Config {rc, sc, tc}) wi key = do
  onlineWordsFetched <- readTVarIO (onlineWordsInProgress tc)
  shouldFetchOffline <- atomically $ setInCharge (offlineWordsInProgress tc) key
  if key `elem` onlineWordsFetched || not shouldFetchOffline then do
    json <- atomically $ fetchFromCache (translationsInCache tc) key
    return $ translationsFromValue json wi
  else do
    available <- (isJust . (HM.lookup key)) <$> readTVarIO (translationsInCache tc)
    if available then do
      offline conf wi key
    else do
      let offlineRequest = currentNbOfOfflineRequest tc
      atomically $ do
        waitForPool offlineRequest $ pgPool sc
        acquireRequestPool offlineRequest
      infoM $ "fetching offline [" <> show key <> "]"
      json <- DB.select conf key
      atomically $ releaseRequestPool offlineRequest
      atomically $ writeOnCache conf (key, json)
      offline conf wi key

online :: Config -> WordInfos -> Word -> IO [Word]
online conf@(Config {rc, sc, tc}) wi key = do
  shouldFetchOnline <- atomically $ setInCharge (onlineWordsInProgress tc) key
  if shouldFetchOnline then do
    let onlineRequest = currentNbOfOnlineRequest tc
    atomically $ do
      waitForPool onlineRequest $ wordReferencePool sc
      acquireRequestPool onlineRequest
    response <- WRStrategy.fetch conf key
    atomically $ releaseRequestPool onlineRequest
    let bytestring = response >>= body :: Maybe BSL.ByteString
    let json = bytestring >>= decode :: Maybe Value
    atomically $ do
      writeOnCache conf (key, json)
      writeOnDB conf (key, json)
    offline conf wi key
  else
    offline conf wi key

body :: Response BSL.ByteString -> Maybe BSL.ByteString
body response = do
  return $ response ^. responseBody

addRequestPool :: TVar Int -> Int -> STM ()
addRequestPool count n = do
  nbRequest <- readTVar count
  writeTVar count $ nbRequest + n

releaseRequestPool count = addRequestPool count (-1)
acquireRequestPool count = addRequestPool count 1

waitForPool :: TVar Int -> Int -> STM ()
waitForPool count max = do
  nbRequest <- readTVar count
  if nbRequest >= max then
    retry
  else
    return ()

fetchFromCache :: TVar Cache -> Word -> STM (Maybe Value)
fetchFromCache tvCache key = do
  cache <- readTVar tvCache
  case HM.lookup key cache of
    Just json -> return json
    Nothing -> retry

writeOnDB :: Config -> (Word, Maybe Value) -> STM ()
writeOnDB conf@(Config {rc, sc, tc}) (key, value) = do
  when (isJust value) $ do
    responses <- readTVar $ responsesToSave tc
    let newResponsesToSave = HM.insert key value responses
    writeTVar (responsesToSave tc) newResponsesToSave

writeOnCache :: Config -> (Word, Maybe Value) -> STM ()
writeOnCache conf@(Config {rc, sc, tc}) (key, value) = do
  availableWords <- readTVar $ availableWordsInDB tc
  cache <- readTVar $ translationsInCache tc
  let newDB = HM.insert key value cache
  writeTVar (translationsInCache tc) newDB
  writeTVar (availableWordsInDB tc) (key : availableWords)

setInCharge :: TVar TakenCare -> Word -> STM Bool
setInCharge tvTakenCare key = do
  isTakenCare <- isTakenCare tvTakenCare key
  if isTakenCare then
    return False
  else do
    setAsTakenCare tvTakenCare key
    return True

setAsTakenCare :: TVar TakenCare -> Word -> STM ()
setAsTakenCare tvTakenCare key = do
  tc <- readTVar tvTakenCare
  writeTVar tvTakenCare (key : tc)

isTakenCare :: TVar TakenCare -> Word -> STM Bool
isTakenCare takenCare key = do
  tc <- readTVar takenCare
  if key `elem` tc then
    return True
  else
    return False

translationsFromValue :: Maybe Value -> WordInfos -> [Word]
translationsFromValue mValue wi@(_, _, tag, _) =
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
