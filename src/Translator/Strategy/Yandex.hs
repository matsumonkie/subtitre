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

translate :: (Text -> IO (Maybe (Response ByteString))) -> WordInfos -> IO Translations
translate fetch wi@(word, lemma, tag, _)
  | shouldBeTranslated tag = do
      response <- fetch toTranslate
      let translations = (translationsBasedOnTag toTranslate tag) response
      res translations
  | otherwise = res Nothing
  where
    shouldBeTranslated :: Tag -> Bool
    shouldBeTranslated = (flip elem) [Verb, Noun, Adj, Sym, Punct, Propn, Pron, Conj, Adv]
    res x = case x of
      Just y -> return $ mkTranslations wi y
      Nothing -> return $ mkTranslations wi []
    toTranslate = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

fetchTranslations :: StaticConf -> Text -> IO (Maybe (Response ByteString))
fetchTranslations sc toTranslate = do
  let key = yandexApiKey sc
  let url = unpack $ yandexApiUrl sc
  liftIO $ catch (Just <$> (getWith (opts key) url)) handler
  where
    handler :: HttpException -> IO (Maybe (Response ByteString))
    handler ex = return Nothing
    opts key = defaults & param "key"  .~ [key]
                        & param "text" .~ [toTranslate]
                        & param "lang" .~ ["en-fr"]

translationsBasedOnTag :: Text -> Tag -> Maybe (Response ByteString) -> Maybe [Text]
translationsBasedOnTag toTranslate tag response = do
  yDef <- body response >>= decode :: Maybe YDef
  let correctTrs = Prelude.filter (\x -> tag == yTrPos x) $ trs yDef
  return $ Prelude.map yTrText correctTrs
  where
    trs :: YDef -> [YTr]
    trs yDef = yEntries yDef >>= yEntryTr

body :: Maybe (Response ByteString) -> Maybe ByteString
body response = do
  r <- response
  return $ r ^. responseBody
