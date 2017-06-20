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

import qualified Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Data.Int
import Database.PostgreSQL.Simple
import Data.Aeson.Types

translate :: (Text -> IO (Maybe (Response ByteString))) -> WordInfos -> IO Translations
translate fetch wi@(word, lemma, tag, _) = do
  response <- fetch toTranslate :: IO (Maybe (Response ByteString))
  let translations = translationsBasedOnTag toTranslate tag =<< response
  case translations of
    Just trs -> do
      L.infoM $ "online :[" <> show toTranslate <> "] " <> show trs
      let bytestring = response >>= body :: Maybe ByteString
      let value = bytestring >>= decode :: Maybe Value
      when (isJust value) $ do
        L.infoM $ "saving to DB new translation for :[" <> show toTranslate <> "] "
        saveToDB toTranslate (fromJust value)
      return $ mkTranslations wi trs
    Nothing -> do
      L.infoM $ "no translations found for :[" <> show toTranslate <> "] "
      return $ mkTranslations wi []
  where
    toTranslate = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

fetchTranslations :: StaticConf -> Text -> IO (Maybe (Response ByteString))
fetchTranslations sc toTranslate =
  let
    key = (wordReferenceApiKeys sc) !! 0
    urlPrefix = wordReferenceApiUrlPrefix sc
    urlSuffix = wordReferenceApiUrlSuffix sc
    url = urlPrefix <> key <> urlSuffix <> toTranslate
  in do
    catch (Just <$> (getWith defaults (unpack url))) handler
  where
    handler :: HttpException -> IO (Maybe (Response ByteString))
    handler ex = do
      L.errorM $ "could not fetch online translation: " <> show ex
      return Nothing

translationsBasedOnTag :: Text -> Tag -> Response ByteString -> Maybe [Text]
translationsBasedOnTag toTranslate tag response = do
  wrResponse <- body response >>= decode :: Maybe WRResponse
  let translations = allTranslations wrResponse
  let correctTrs = filter (\x -> tag == tPos x) translations
  Just $ if P.null correctTrs then
    P.map tTerm translations
  else
    P.map tTerm correctTrs

body :: Response ByteString -> Maybe ByteString
body response = do
  return $ response ^. responseBody

saveToDB :: Text -> Value -> IO ()
saveToDB word object = do
  con <- connectPostgreSQL config
  now <- getCurrentTime
  execute con q ("en" :: Text, "fr" :: Text, word, object, now, now)
  return ()
  where
    config = "dbname='subtitre_dev'"
    q = "INSERT INTO wordreference (\"from\", \"to\", \"word\", \"response\", \"created_at\", \"updated_at\") \
        \ values (?, ?, ?, ?, ?, ?) "
