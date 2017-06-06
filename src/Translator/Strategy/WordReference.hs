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
import Data.Text
import Data.Maybe
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy hiding (elem, unpack)
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

translate :: (Text -> IO (Maybe (Response ByteString))) -> WordInfos -> IO Translations
translate fetch wi@(word, lemma, tag, _)
  | shouldBeTranslated tag = do
      response <- fetch toTranslate
      let translations = (translationsBasedOnTag toTranslate tag) response
      res translations
  | otherwise = res Nothing
  where
    shouldBeTranslated :: Tag -> Bool
    shouldBeTranslated = (flip elem) [Verb, Noun, Adj]
    res x = case x of
      Just y -> return $ mkTranslations wi y
      Nothing -> return $ mkTranslations wi []
    toTranslate = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

fetchTranslations :: StaticConf -> Text -> IO (Maybe (Response ByteString))
fetchTranslations sc toTranslate =
  let
    key = wordReferenceApiKey sc
    urlPrefix = wordReferenceApiUrlPrefix sc
    urlSuffix = wordReferenceApiUrlSuffix sc
    url = urlPrefix <> key <> urlSuffix <> toTranslate
  in
    catch (Just <$> (getWith defaults (unpack url))) handler
  where
    handler :: HttpException -> IO (Maybe (Response ByteString))
    handler ex = return Nothing

translationsBasedOnTag :: Text -> Tag -> Maybe (Response ByteString) -> Maybe [Text]
translationsBasedOnTag toTranslate tag response = do
  wrResponse <- body response >>= decode :: Maybe WRResponse
  let correctTrs = Prelude.filter (\x -> tag == tPos x) $ allWrTranslationsTerms wrResponse
  return $ Prelude.map tTerm correctTrs

allWrTranslationsTerms :: WRResponse -> [WRTranslation]
allWrTranslationsTerms wrResponse =
  terms wrResponse >>= principalTranslations

body :: Maybe (Response ByteString) -> Maybe ByteString
body response = do
  r <- response
  return $ r ^. responseBody
