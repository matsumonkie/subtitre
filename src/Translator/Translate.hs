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

translate :: WordInfos -> App Translations
translate wi =
  liftA2 Type.or (offline wi) (online wi)

offline :: WordInfos -> App Translations
offline = Offline.translate

online :: WordInfos -> App Translations
online = Yandex.translate Yandex.fetchTranslations
--online = WordReference.translate WordReference.fetchTranslations
