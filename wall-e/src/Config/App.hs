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
, askS
, RuntimeConf(..)
, inputFile
, outputFile
, StaticConf(..)
, getStaticConf
, Translator
) where

import Common
import Prelude()

import Control.DeepSeq
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.ByteString as BS
import Data.Maybe
import Data.Text
import qualified Data.Yaml as Y
import GHC.Generics (Generic)
import System.Log.Logger
import Text.Parsec
import Type


{- RUNTIME CONF -}


instance NFData Priority where
  rnf x = ()

data RuntimeConf =
  RuntimeConf { translator :: Translator
              , levelSets :: LevelSets
              , levelToShow :: Level
              , dir :: FilePath
              , file :: FilePath
              , logLevel :: Priority
              , logFormatter :: String
              } deriving (Generic, NFData)

type Translator = TP -> StaticConf -> WordInfos -> IO Translations

inputFile :: RuntimeConf -> FilePath
inputFile conf = (dir conf) <> "/" <> (file conf)

outputFile :: RuntimeConf -> FilePath
outputFile conf = (dir conf) <> "/t" <> (file conf)


{- STATIC CONF -}


data StaticConf =
  StaticConf { database :: Text
             , workingDir :: FilePath
             , outputFileName :: Text
             , pgPool :: Int
             -- yandex
             , yandexApiKey :: Text
             , yandexApiUrl :: Text
             -- word reference
             , wordReferenceApiUrlPrefix :: Text
             , wordReferenceApiUrlSuffix :: Text
             , wordReferenceApiKeys :: [Text]
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


{- APP CONF -}


data Config = Config (RuntimeConf, StaticConf) deriving (Generic, NFData)

data AppError = AppError ParseError deriving (Show, Eq)
type App a = ReaderT Config (ExceptT [AppError] IO) a

asksR :: (Monad m) => (RuntimeConf -> a) -> ReaderT Config m a
asksR f = do
  (Config(config, _)) <- ask
  return $ f config

asksS :: (Monad m) => (StaticConf -> a) -> ReaderT Config m a
asksS f = do
  (Config(_, config)) <- ask
  return $ f config

askS :: (Monad m) => ReaderT Config m StaticConf
askS = do
  (Config(_, config)) <- ask
  return config
