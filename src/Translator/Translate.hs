{-# LANGUAGE OverloadedStrings #-}

module Translator.Translate (
  translate
, translateOffline
, translateOnline
) where

import Type
import Data.Text
import Data.Maybe
import qualified Translator.Online as Online
import qualified Translator.Offline as Offline

translate :: WordInfos -> IO Translation
translate wordInfos = do
  offline@(_, mTranslation) <- translateOffline wordInfos
  online <- translateOnline wordInfos
  return $ if (isJust mTranslation) then
    offline
  else
    online

translateOffline :: WordInfos -> IO Translation
translateOffline = Offline.translate

translateOnline :: WordInfos -> IO Translation
translateOnline = Online.translate
