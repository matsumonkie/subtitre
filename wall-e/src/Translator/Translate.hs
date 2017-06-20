{-# LANGUAGE DeriveFunctor #-}
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
import qualified Logger as L
import Debug.Trace
import Text.Pretty.Simple (pPrint, pString)
import Prelude as P

translate :: StaticConf -> WordInfos -> IO Translations
translate sc wi = do
  off@(Translations' (_, a1)) <- offline sc wi
  if (not . P.null) a1 then
    return off
  else
    online sc wi

offline :: StaticConf -> WordInfos -> IO Translations
offline =
  Offline.translate

online :: StaticConf -> WordInfos -> IO Translations
online sc =
  WordReference.translate (WordReference.fetchTranslations sc)
