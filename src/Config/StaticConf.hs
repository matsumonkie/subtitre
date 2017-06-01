{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.StaticConf (
  StaticConf(..)
, getStaticConf
) where

import Data.Aeson
import qualified Data.Yaml as Y
import Data.Text
import Data.Either
import Data.ByteString hiding (putStrLn)
import Prelude hiding (readFile)
import Data.Monoid
import Data.Maybe

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
