{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Config.App (
  App
, AppError(..)
, Config(..)
, asksR
, asksS
, asksT
, askS
, RuntimeConf(..)
, StaticConf(..)
, getStaticConf
, TranslationsConf(..)
, TP(..)
) where

import Common
import Prelude()

import Control.Concurrent.STM
import Control.DeepSeq
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import System.Log.Logger
import Text.Parsec
import Type


{- RUNTIME CONF -}


data RuntimeConf =
  RuntimeConf { levelSets :: LevelSets
              , levelToShow :: Level
              , file :: FilePath
              , fromLang :: T.Text
              , toLang :: T.Text
              , dontTranslate :: TextSet
              , logLevel :: Priority
              , logFormatter :: String
              }

instance Show RuntimeConf where
  show rc =
    (show $ file rc) <> "\n" <>
    (show $ toLang rc) <> "\n" <>
    (show $ levelToShow rc) <> "\n" <>
    (show $ logLevel rc) <> "\n" <>
    (show $ logFormatter rc)

type Translator = Config -> WordInfos -> IO Translations
type TP a = ReaderT Config IO a


{- STATIC CONF -}


data StaticConf =
  StaticConf { database :: T.Text
             , workingDir :: FilePath
             , outputFileName :: T.Text
             , pgPool :: Int
             -- yandex
             , yandexApiKey :: T.Text
             , yandexApiUrl :: T.Text
             -- word reference
             , wordReferenceApiUrlPrefix :: T.Text
             , wordReferenceApiUrlSuffix :: T.Text
             , wordReferenceApiKeys :: [T.Text]
             , wordReferencePool :: Int
             } deriving (Show, Generic, NFData)

instance FromJSON StaticConf where
  parseJSON (Y.Object v) = do
    config <- v .: "development"
    database <- config .: "database"
    workingDir <- config .: "workingDir"
    outputFileName <- config .: "outputFileName"
    pgPool <- config .: "pgPool"
    yandexApiKey <- config .: "yandexApiKey"
    yandexApiUrl <- config .: "yandexApiUrl"
    wordReferenceApiUrlPrefix <- config .: "wordReferenceApiUrlPrefix"
    wordReferenceApiUrlSuffix <- config .: "wordReferenceApiUrlSuffix"
    wordReferenceApiKeys <- config .: "wordReferenceApiKeys"
    wordReferencePool <- config .: "wordReferencePool"
    return StaticConf {..}
  parseJSON _ = fail "Expected Object for Config value"

getStaticConf :: IO StaticConf
getStaticConf =
  fromJust <$> Y.decode <$> readStatic "./settings.yml"
  where
    readStatic :: FilePath -> IO BS.ByteString
    readStatic file = BS.readFile file


{- TRANSLATIONS CONF -}


data TranslationsConf =
  TranslationsConf { responsesToSave :: TVar Cache
                   , currentNbOfOnlineRequest :: TVar Int
                   }


{- APP CONF -}


data Config = Config { rc :: RuntimeConf
                     , sc :: StaticConf
                     , tc :: TranslationsConf
                     }

data AppError = AppError ParseError deriving (Show, Eq)
type App a = ReaderT Config (ExceptT [AppError] IO) a

asksR :: (Monad m) => (RuntimeConf -> a) -> ReaderT Config m a
asksR f = do
  (Config { rc, sc, tc }) <- ask
  return $ f rc

asksS :: (Monad m) => (StaticConf -> a) -> ReaderT Config m a
asksS f = do
  (Config { rc, sc, tc }) <- ask
  return $ f sc

asksT :: (Monad m) => (TranslationsConf -> a) -> ReaderT Config m a
asksT f = do
  (Config { rc, sc, tc }) <- ask
  return $ f tc

askS :: (Monad m) => ReaderT Config m StaticConf
askS = do
  (Config { rc, sc, tc }) <- ask
  return sc
