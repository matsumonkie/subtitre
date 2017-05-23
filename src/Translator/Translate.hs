{-# LANGUAGE OverloadedStrings #-}

module Translator.Translate (
  translate
, offline
, online
) where

import Type
import Data.Text
import qualified Translator.Online as Online
import qualified Translator.Offline as Offline
import Control.Applicative

translate :: WordInfos -> IO Translations
translate wi =
  liftA2 Type.or (offline wi) (online wi)

offline :: WordInfos -> IO Translations
offline = Offline.translate

online :: WordInfos -> IO Translations
online = \wi -> Online.translate wi Online.fetchTranslations
