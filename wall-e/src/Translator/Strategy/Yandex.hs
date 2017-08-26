{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator.Strategy.Yandex (
  fetch
) where

import Type
import Serializer
import Data.Text
import Data.Maybe
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import GHC.Generics
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Text.Pretty.Simple (pPrint, pString)
import GHC.Exts
import Debug.Trace
import Control.Exception
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.Either
import Deserializer.Yandex
import Config.App
import Control.Monad.IO.Class

fetch :: StaticConf -> Text -> IO (Maybe (Response BSL.ByteString))
fetch sc toTranslate = do
  let key = yandexApiKey sc
  let url = unpack $ yandexApiUrl sc
  liftIO $ catch (Just <$> (getWith (opts key) url)) handler
  where
    handler :: HttpException -> IO (Maybe (Response BSL.ByteString))
    handler ex = return Nothing
    opts key = defaults & param "key"  .~ [key]
                        & param "text" .~ [toTranslate]
                        & param "lang" .~ ["en-fr"]
