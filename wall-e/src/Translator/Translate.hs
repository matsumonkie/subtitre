{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Translator.Translate (
  translate
) where

import Type
import Data.Text
import qualified Translator.Offline as Offline
import Control.Applicative
import qualified Translator.Strategy.Yandex as Yandex
import qualified Translator.Strategy.WordReference as WordReference
import Config.App
import Data.Monoid
import qualified Logger as L
import Debug.Trace
import Text.Pretty.Simple (pPrint, pString)
import Prelude as P
import Control.Concurrent.STM
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as B hiding (elem, unpack, filter, map)
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.ByteString.Lazy hiding (elem, unpack, filter, map)
import Network.Wreq
import Type
import Serializer
import Data.Text hiding (filter, map)
import Data.Maybe
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy hiding (elem, unpack, filter, map)
import Text.Pretty.Simple (pPrint, pString)
import GHC.Exts
import Debug.Trace
import Control.Exception
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.Either
import Deserializer.Yandex
import Config.App
import Data.Monoid
import Control.Monad.IO.Class
import Deserializer.WordReference
import qualified Logger as L
import Prelude as P
import Control.Concurrent.Thread.Delay
import Data.Int
import Control.Monad
import Data.Time.Clock
import Type
import Serializer
import Data.Text hiding (filter, map)
import Data.Maybe
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy hiding (elem, unpack, filter, map)
import Text.Pretty.Simple (pPrint, pString)
import GHC.Exts
import Debug.Trace
import Control.Exception
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.Either
import Deserializer.Yandex
import Config.App
import Data.Monoid
import Control.Monad.IO.Class
import Deserializer.WordReference
import qualified Logger as L
import Prelude as P
import Control.Concurrent.Thread.Delay
import Data.Int
import Control.Monad
import Data.Time.Clock
import qualified DB.WordReference as DB


translate :: TP -> StaticConf -> WordInfos -> IO Translations
translate tp sc wi@(_, _, tag, _) = do
  mkTranslations wi <$> translate' tp sc wi (toTranslate wi)
  where
    toTranslate :: WordInfos -> Text
    toTranslate (word, lemma, tag, _) = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

translate' :: TP -> StaticConf -> WordInfos -> Text -> IO [Text]
translate' tp sc wi@(_, _, tag, _) key = do
  availableWordsInDB' <- readTVarIO (availableWordsInDB tp)
  if key `elem` availableWordsInDB' then
    offline tp sc wi key
  else
    online tp sc wi key

delayS :: Integer -> IO ()
delayS n =
  delay $ n * 1000 * 1000

offline :: TP -> StaticConf -> WordInfos -> Text -> IO [Text]
offline tp sc wi@(_, _, tag, _) key = do
  shouldFetchOffline <- atomically $ setInCharge (offlineWordsInProgress tp) key
  if shouldFetchOffline then do
    cache <- readTVarIO (translationsInCache tp)
    let available = isJust $ HM.lookup key cache
    if available then do
      offline tp sc wi key
    else do
      let offlineRequest = currentNbOfOfflineRequest tp
      atomically $ do
        waitForPool offlineRequest 5
        acquireRequestPool offlineRequest
      json <- DB.select sc key
      atomically $ releaseRequestPool offlineRequest
      atomically $ writeOnCache tp (key, json)
      offline tp sc wi key
  else do
    json <- atomically $ fetchFromCache (translationsInCache tp) key
    return $ translationsFromValue json wi

online :: TP -> StaticConf -> WordInfos -> Text -> IO [Text]
online tp sc wi@(_, _, tag, _) key = do
  shouldFetchOnline <- atomically $ setInCharge (onlineWordsInProgress tp) key
  if shouldFetchOnline then do
    let onlineRequest = currentNbOfOnlineRequest tp
    atomically $ do
      waitForPool onlineRequest 5
      acquireRequestPool onlineRequest
    response <- fetchOnline sc key
    atomically $ releaseRequestPool onlineRequest
    let bytestring = response >>= body :: Maybe ByteString
    let json = bytestring >>= decode :: Maybe Value
    atomically $ do
      writeOnCache tp (key, json)
      writeOnDB tp (key, json)
    offline tp sc wi key
  else
    offline tp sc wi key

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

fetchFromCache :: TVar Cache -> Text -> STM (Maybe Value)
fetchFromCache tvCache key = do
  cache <- readTVar tvCache
  case HM.lookup key cache of
    Just json -> return json
    Nothing -> retry

writeOnDB :: TP -> (Text, Maybe Value) -> STM ()
writeOnDB tp (key, value) = do
  when (isJust value) $ do
    responses <- readTVar (responsesToSave tp)
    let newResponsesToSave = HM.insert key value responses
    writeTVar (responsesToSave tp) newResponsesToSave

writeOnCache :: TP -> (Text, Maybe Value) -> STM ()
writeOnCache tp (key, value) = do
  availableWords <- readTVar (availableWordsInDB tp)
  cache <- readTVar (translationsInCache tp)
  let newDB = HM.insert key value cache
  writeTVar (translationsInCache tp) newDB
  writeTVar (availableWordsInDB tp) (key : availableWords)

setInCharge :: TVar TakenCare -> Text -> STM Bool
setInCharge tvTakenCare key = do
  isTakenCare <- isTakenCare tvTakenCare key
  if isTakenCare then
    return False
  else do
    setAsTakenCare tvTakenCare key
    return True

setAsTakenCare :: TVar TakenCare -> Text -> STM ()
setAsTakenCare tvTakenCare key = do
  tc <- readTVar tvTakenCare
  writeTVar tvTakenCare (key : tc)

isTakenCare :: TVar TakenCare -> Text -> STM Bool
isTakenCare takenCare key = do
  tc <- readTVar takenCare
  if key `elem` tc then
    return True
  else
    return False

translationsFromValue :: Maybe Value -> WordInfos -> [Text]
translationsFromValue mValue wi@(_, _, tag, _) =
  maybe [] (translationsBasedOnTag tag) mValue

body :: Response ByteString -> Maybe ByteString
body response = do
  return $ response ^. responseBody

fetchOnline :: StaticConf -> Text -> IO (Maybe (Response ByteString))
fetchOnline sc toTranslate =
  let
    key = (wordReferenceApiKeys sc) !! 0
    urlPrefix = wordReferenceApiUrlPrefix sc
    urlSuffix = wordReferenceApiUrlSuffix sc
    url = urlPrefix <> key <> urlSuffix <> toTranslate
  in do
    L.infoM $ "fetching online [" <> show toTranslate <> "]"
    catch (Just <$> (getWith defaults (unpack url))) handler
  where
    handler :: SomeException -> IO (Maybe (Response ByteString))
    handler ex = do
      L.errorM $ "could not fetch online translation: [" <> show toTranslate <> "] with exception : " <> show ex
      return Nothing

translationsBasedOnTag :: Tag -> Value -> [Text]
translationsBasedOnTag tag value = do
  case (fromJSON value :: Result WRResponse) of
    Error _ -> []
    Success wrResponse -> do
      let translations = allTranslations wrResponse
      let correctTrs = P.filter (\x -> tag == tPos x) translations
      if P.null correctTrs then
        P.map tTerm translations
      else
        P.map tTerm correctTrs
