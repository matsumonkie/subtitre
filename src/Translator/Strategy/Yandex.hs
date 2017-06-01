{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator.Strategy.Yandex (
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
import Data.ByteString.Lazy hiding (elem, pack, unpack)
import Text.Pretty.Simple (pPrint, pString)
import GHC.Exts
import Debug.Trace
import Control.Exception
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.Either
import Deserializer.Yandex
import Config.App
import Control.Monad.IO.Class

translate :: (Text -> IO (Maybe (Response ByteString))) -> WordInfos -> App Translations
translate fetch wi@(word, lemma, tag, _)
  | shouldBeTranslated tag = do
      response <- liftIO $ fetch toTranslate
      let translations = (translationsBasedOnTag toTranslate tag) response
      res translations
  | otherwise = res Nothing
  where
    shouldBeTranslated :: RealTag -> Bool
    shouldBeTranslated = (flip elem) [Verb, Noun, Adj, Sym, Punct, Propn, Pron, Conj, Adv]
    res x = case x of
      Just y -> return $ mkTranslations wi y
      Nothing -> return $ mkTranslations wi []
    toTranslate = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

fetchTranslations :: Text -> App (Maybe (Response ByteString))
fetchTranslations toTranslate = do
  key <- askS yandexApiKey
  url <- unpack <$> askS yandexApiUrl
  liftIO $ catch (Just <$> (getWith (opts key) url)) handler
  where
    handler :: HttpException -> IO (Maybe (Response ByteString))
    handler ex = return Nothing
    opts key = defaults & param "key"  .~ [key]
                        & param "text" .~ [toTranslate]
                        & param "lang" .~ ["en-fr"]

translationsBasedOnTag :: Text -> RealTag -> Maybe (Response ByteString) -> Maybe [Text]
translationsBasedOnTag toTranslate tag response = do
  yDef <- body response >>= decode :: Maybe YDef
  let correctTrs = Prelude.filter (\x -> tag == yTrPos x) $ trs yDef
  return $ Prelude.map yTrText correctTrs
  where
    trs :: YDef -> [YTr]
    trs yDef = Prelude.concat $ Prelude.map yEntryTr (yEntries yDef)

body :: Maybe (Response ByteString) -> Maybe ByteString
body response = do
  r <- response
  return $ r ^. responseBody
