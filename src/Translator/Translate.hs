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

translate :: WordInfos -> IO Translations
translate wi =
  liftA2 Type.or (offline wi) (online wi)

offline :: WordInfos -> IO Translations
offline = Offline.translate

online :: WordInfos -> IO Translations
online = Yandex.translate Yandex.fetchTranslations
