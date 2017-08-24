module DontTranslate (
  dontTranslate
) where

import Common
import Prelude()

import qualified Data.HashSet as HS
import Type
import AssetAsSet

dontTranslate :: Language -> Language -> IO TextSet
dontTranslate fromLang toLang = do
  let specificToLanguage = fromLang <> toLang
  specificToTargetedLanguage <- assetAsSet $ "donttranslate/" <> fromLang <> toLang
  let filePath = assetPath $ "donttranslate/" <> fromLang
  general <- assetAsSet $ "donttranslate/" <> fromLang
  return $ specificToTargetedLanguage `HS.union` general
