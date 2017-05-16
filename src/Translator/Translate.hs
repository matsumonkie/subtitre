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
import Control.Concurrent.Async

translate :: WordInfos -> IO [Text]
translate wi =
  liftA2 (<|>) (offline wi) (online wi)

offline :: WordInfos -> IO [Text]
offline = Offline.translate

online :: WordInfos -> IO [Text]
online = Online.translate
