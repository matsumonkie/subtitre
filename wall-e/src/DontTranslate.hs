module DontTranslate (
  dontTranslate
) where

import Common
import Prelude()

import qualified Data.HashSet as HS
import qualified Data.Text as T
import Type
import AssetAsSet

dontTranslate :: Language -> Language -> IO TextSet
dontTranslate fromLang toLang = do
  let specificToLanguage = fromLang <> toLang
  specificToTargetedLanguage <-
    assetAsSet $ "donttranslate/" <> T.unpack fromLang <> T.unpack toLang
  let filePath = assetPath $ "donttranslate/" <> T.unpack fromLang
  general <- assetAsSet $ "donttranslate/" <> T.unpack fromLang
  return $ specificToTargetedLanguage `HS.union` general
