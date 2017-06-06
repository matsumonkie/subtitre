{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

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
) where

import Type
import Text.Parsec
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Type
import Data.Aeson
import qualified Data.Yaml as Y
import Data.Text
import Data.Either
import Data.ByteString hiding (putStrLn)
import Prelude hiding (readFile)
import Data.Monoid
import Data.Maybe


{- RUNTIME CONF -}


data RuntimeConf =
  RuntimeConf { translator :: Translator
          , levelSets :: LevelSets
          , levelToShow :: Level
          , dir :: FilePath
          , file :: FilePath
          }

type Translator = StaticConf -> WordInfos -> IO Translations

inputFile :: RuntimeConf -> FilePath
inputFile conf = (dir conf) <> "/" <> (file conf)

outputFile :: RuntimeConf -> FilePath
outputFile conf = (dir conf) <> "/t" <> (file conf)


{- STATIC CONF -}


data StaticConf =
  StaticConf { workingDir :: FilePath
             , outputFileName :: Text
             -- yandex
             , yandexApiKey :: Text
             , yandexApiUrl :: Text
             -- word reference
             , wordReferenceApiUrlPrefix :: Text
             , wordReferenceApiUrlSuffix :: Text
             , wordReferenceApiKey :: Text
             } deriving Show

instance FromJSON StaticConf where
  parseJSON (Y.Object v) = do
    config <- v .: "development"
    workingDir <- config .: "workingDir"
    outputFileName <- config .: "outputFileName"
    yandexApiKey <- config .: "yandexApiKey"
    yandexApiUrl <- config .: "yandexApiUrl"
    wordReferenceApiUrlPrefix <- config .: "wordReferenceApiUrlPrefix"
    wordReferenceApiUrlSuffix <- config .: "wordReferenceApiUrlSuffix"
    wordReferenceApiKey <- config .: "wordReferenceApiKey"
    return StaticConf {..}

getStaticConf :: IO StaticConf
getStaticConf =
  fromJust <$> Y.decode <$> readStatic "settings.yml"
  where
    readStatic :: FilePath -> IO ByteString
    readStatic file = readFile file


{- APP CONF -}


data Config = Config (RuntimeConf, StaticConf)

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
