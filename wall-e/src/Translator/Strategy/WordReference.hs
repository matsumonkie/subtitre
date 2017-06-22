{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator.Strategy.WordReference (
  translate
, fetchTranslations
) where

import Common
import Prelude()

import Config.App
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS hiding (elem, unpack, filter, map)
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Deserializer.WordReference
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Network.Wreq
import Type

translate :: (Word -> IO (Maybe (Response LBS.ByteString))) -> WordInfos -> IO Translations
translate fetch wi@(word, lemma, tag, _) = do
  response <- fetch toTranslate :: IO (Maybe (Response LBS.ByteString))
  let translations = translationsBasedOnTag toTranslate tag =<< response
  case translations of
    Just trs -> do
      infoM $ "online :[" <> show toTranslate <> "] " <> show trs
      let bytestring = response >>= body :: Maybe LBS.ByteString
      let value = bytestring >>= decode :: Maybe Value
      when (isJust value) $ do
        infoM $ "saving to DB new translation for :[" <> show toTranslate <> "] "
        saveToDB toTranslate (fromJust value)
      return $ mkTranslations wi trs
    Nothing -> do
      infoM $ "no translations found for :[" <> show toTranslate <> "] "
      return $ mkTranslations wi []
  where
    toTranslate = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

fetchTranslations :: StaticConf -> Word -> IO (Maybe (Response LBS.ByteString))
fetchTranslations sc toTranslate =
  let
    key = (wordReferenceApiKeys sc) !! 0
    urlPrefix = wordReferenceApiUrlPrefix sc
    urlSuffix = wordReferenceApiUrlSuffix sc
    url = urlPrefix <> key <> urlSuffix <> toTranslate
  in do
    catch (Just <$> (getWith defaults (T.unpack url))) handler
  where
    handler :: HttpException -> IO (Maybe (Response LBS.ByteString))
    handler ex = do
      errorM $ "could not fetch online translation: " <> show ex
      return Nothing

translationsBasedOnTag :: Word -> Tag -> Response LBS.ByteString -> Maybe [Word]
translationsBasedOnTag toTranslate tag response = do
  wrResponse <- body response >>= decode :: Maybe WRResponse
  let translations = allTranslations wrResponse
  let correctTrs = filter (\x -> tag == tPos x) translations
  Just $ if null correctTrs then
    map tTerm translations
  else
    map tTerm correctTrs

body :: Response LBS.ByteString -> Maybe LBS.ByteString
body response = do
  return $ response ^. responseBody

saveToDB :: Word -> Value -> IO ()
saveToDB word object = do
  con <- connectPostgreSQL config
  now <- getCurrentTime
  execute con q ("en" :: Word, "fr" :: Word, word, object, now, now)
  return ()
  where
    config = "dbname='subtitre_dev'"
    q = "INSERT INTO wordreference (\"from\", \"to\", \"word\", \"response\", \"created_at\", \"updated_at\") \
        \ values (?, ?, ?, ?, ?, ?) "
