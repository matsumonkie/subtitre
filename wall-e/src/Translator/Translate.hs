{-# LANGUAGE OverloadedStrings #-}

module Translator.Translate (
  translate
, offline
, online
) where

import Type
import Data.Text
import qualified Translator.Offline as Offline
import Control.Applicative
import qualified Translator.Strategy.Yandex as Yandex
import qualified Translator.Strategy.WordReference as WordReference
import Config.App
import Data.Monoid

translate :: StaticConf -> WordInfos -> IO Translations
translate sc wi =
  liftA2 (<>) (offline sc wi) (online sc wi)

offline :: StaticConf -> WordInfos -> IO Translations
offline = Offline.translate

online :: StaticConf -> WordInfos -> IO Translations
--online sc = Yandex.translate (Yandex.fetchTranslations sc)
online sc = WordReference.translate (WordReference.fetchTranslations sc)
