{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Translator.Offline (
  translate
) where

import Data.Text.Lazy.Encoding

import Type
import Data.Text hiding (filter, null, map)
import Data.Maybe
import Prelude hiding (lookup)
import Control.Applicative
import Data.Functor.Identity
import Text.Pretty.Simple (pPrint, pString)
import Config.App
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Deserializer.WordReference
import qualified Logger as L
import Control.Monad.IO.Class
import Control.Monad
import Data.Monoid
import Data.Int
import Data.ByteString.Lazy hiding (elem, unpack, filter, map, null)
import qualified Data.ByteString as B hiding (elem, unpack, filter, map)
import Data.Aeson
import Data.Aeson.Types
import Database.PostgreSQL.Simple.ToField
import qualified Data.HashMap.Strict as HM
import qualified Data.Text.Encoding as Encoding

translate :: StaticConf -> WordInfos -> IO Translations
translate sc wi@(word, lemma, tag, level) = do
  response <- fetch sc toTranslate :: IO (Maybe Value)
  let wrResponse = response >>= parseMaybe (parseJSON) :: Maybe WRResponse
  let translations = translationsBasedOnTag toTranslate tag <$> wrResponse :: Maybe [Text]
  case translations of
    Just trs -> do
      L.infoM $ "offline :[" <> show toTranslate <> "] " <> show trs
      return $ mkTranslations wi trs
    Nothing ->
      return $ mkTranslations wi []
  where
    toTranslate = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

translationsBasedOnTag :: Text -> Tag -> WRResponse -> [Text]
translationsBasedOnTag toTranslate tag wrResponse = do
  let translations = allTranslations wrResponse
  let correctTrs = filter (\x -> tag == tPos x) translations
  if null correctTrs then
    map tTerm translations
  else
    map tTerm correctTrs

resToMaybe :: [Only a] -> Maybe a
resToMaybe responses =
  case responses of
    [Only i] -> Just i
    _ -> Nothing

fetch :: StaticConf -> Text -> IO (Maybe Value)
fetch sc word = do
  con <- connectPostgreSQL config
  responses <- query con q (Only word) :: IO ([Only Value])
  return $ resToMaybe responses
  where
    config :: B.ByteString
    config = B.append (B.append "dbname='" (Encoding.encodeUtf8 $ database sc)) "'"
    q = "SELECT response \
        \FROM wordreference w \
        \WHERE w.from = 'en' AND w.to = 'fr' AND w.word = ? \
        \LIMIT 1"
