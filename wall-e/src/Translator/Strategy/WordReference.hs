{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator.Strategy.WordReference (
  fetch
) where

import Common
import Prelude()

import Config.App
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock
import Database.PostgreSQL.Simple
import Deserializer.WordReference
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Network.Wreq
import Network.Wreq.Types as Wreq
import Type
import Network.HTTP.Client

fetch :: Config -> Word -> IO (Maybe (Response BSL.ByteString))
fetch conf@(Config {rc, sc, tc}) toTranslate =
  let
    key = (wordReferenceApiKeys sc) !! 0
    urlPrefix = wordReferenceApiUrlPrefix sc
    urlSuffix = wordReferenceApiUrlSuffix sc <> T.pack (toLang rc) <> "/"
    url = urlPrefix <> key <> urlSuffix <> toTranslate
    opts = defaults { Wreq.redirects = 2 }
  in do
    infoM $ "fetching online [" <> show toTranslate <> "]"
    catch (Just <$> (getWith opts (T.unpack url))) handler
  where
    handler :: HttpException -> IO (Maybe (Response BSL.ByteString))
    handler ex = do
      case ex of
        HttpExceptionRequest _ (TooManyRedirects _) -> do
          errorM $ "too many redirects, could not fetch online translation: [" <> show toTranslate
          return Nothing
        _ -> do
          errorM $ "could not fetch online translation: [" <> show toTranslate <> "] with exception : " <> show ex
          return Nothing
