{-# LANGUAGE OverloadedStrings #-}
module Translator.Translate (
  translate
) where

import Common
import Prelude()

import Config.App
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
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

translate :: TP -> Language -> StaticConf -> WordInfos -> IO Translations
translate tp to sc wi@(_, _, tag, _) = do
  mkTranslations wi <$> translate' tp to sc wi (toTranslate wi)
  where
    toTranslate :: WordInfos -> Word
    toTranslate (word, lemma, tag, _) = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

translate' :: TP -> Language -> StaticConf -> WordInfos -> Word -> IO [Word]
translate' tp to sc wi@(_, _, tag, _) key = do
  availableWordsInDB' <- readTVarIO (availableWordsInDB tp)
  if key `elem` availableWordsInDB' then
    offline tp to sc wi key
  else
    online tp to sc wi key

offline :: TP -> Language -> StaticConf -> WordInfos -> Word -> IO [Word]
offline tp to sc wi@(_, _, tag, _) key = do
  onlineWordsFetched <- readTVarIO (onlineWordsInProgress tp)
  shouldFetchOffline <- atomically $ setInCharge (offlineWordsInProgress tp) key
  if key `elem` onlineWordsFetched || not shouldFetchOffline then do
    json <- atomically $ fetchFromCache (translationsInCache tp) key
    return $ translationsFromValue json wi
  else do
    cache <- readTVarIO (translationsInCache tp)
    let available = isJust $ HM.lookup key cache
    if available then do
      offline tp to sc wi key
    else do
      let offlineRequest = currentNbOfOfflineRequest tp
      atomically $ do
        waitForPool offlineRequest 5
        acquireRequestPool offlineRequest
      json <- DB.select sc to key
      atomically $ releaseRequestPool offlineRequest
      atomically $ writeOnCache tp (key, json)
      offline tp to sc wi key

online :: TP -> Language -> StaticConf -> WordInfos -> Word -> IO [Word]
online tp to sc wi@(_, _, tag, _) key = do
  shouldFetchOnline <- atomically $ setInCharge (onlineWordsInProgress tp) key
  if shouldFetchOnline then do
    let onlineRequest = currentNbOfOnlineRequest tp
    atomically $ do
      waitForPool onlineRequest 5
      acquireRequestPool onlineRequest
    response <- fetchOnline sc to key
    atomically $ releaseRequestPool onlineRequest
    let bytestring = response >>= body :: Maybe BSL.ByteString
    let json = bytestring >>= decode :: Maybe Value
    atomically $ do
      writeOnCache tp (key, json)
      writeOnDB tp (key, json)
    offline tp to sc wi key
  else
    offline tp to sc wi key

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

writeOnDB :: TP -> (Word, Maybe Value) -> STM ()
writeOnDB tp (key, value) = do
  when (isJust value) $ do
    responses <- readTVar (responsesToSave tp)
    let newResponsesToSave = HM.insert key value responses
    writeTVar (responsesToSave tp) newResponsesToSave

writeOnCache :: TP -> (Word, Maybe Value) -> STM ()
writeOnCache tp (key, value) = do
  availableWords <- readTVar (availableWordsInDB tp)
  cache <- readTVar (translationsInCache tp)
  let newDB = HM.insert key value cache
  writeTVar (translationsInCache tp) newDB
  writeTVar (availableWordsInDB tp) (key : availableWords)

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

body :: Response BSL.ByteString -> Maybe BSL.ByteString
body response = do
  return $ response ^. responseBody

fetchOnline :: StaticConf -> Language -> Word -> IO (Maybe (Response BSL.ByteString))
fetchOnline sc to toTranslate =
  let
    key = (wordReferenceApiKeys sc) !! 0
    urlPrefix = wordReferenceApiUrlPrefix sc
    urlSuffix = wordReferenceApiUrlSuffix sc <> T.pack to <> "/"
    url = urlPrefix <> key <> urlSuffix <> toTranslate
  in do
    infoM $ "fetching online [" <> show toTranslate <> "]"
    catch (Just <$> (getWith defaults (T.unpack url))) handler
  where
    handler :: SomeException -> IO (Maybe (Response BSL.ByteString))
    handler ex = do
      errorM $ "could not fetch online translation: [" <> show toTranslate <> "] with exception : " <> show ex
      return Nothing

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
