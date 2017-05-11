{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator.Online (
  translate
) where

import Type
import Serializer
import Data.Text
import Data.Maybe
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy
import Text.Pretty.Simple (pPrint, pString)
import GHC.Exts

translate :: WordInfos -> IO Translation
translate wi@(word, lemma, tag) = do
  translations <- translationsBasedOnTag toTranslate tag
  return (wi, listToMaybe translations)
  where
    toTranslate = case tag of
      Verb -> lemma
      _ -> word

data Def =
  Def { text :: Text
      , pos :: Tag
      , tr :: [Tr]
      } deriving (Show, Generic)

data Tr = Tr Text deriving (Show)

getText :: Tr -> Text
getText (Tr text) = text

instance FromJSON Def
instance FromJSON Tr where
  parseJSON = withObject "tr" $ \o -> do
    text <- o .: "text"
    return $ Tr text

translationsBasedOnTag :: Text -> Tag -> IO [Text]
translationsBasedOnTag toTranslate tag = do
  translations <- fromJSON <$> (fetchTranslations toTranslate)
  let correctDefs = Prelude.filter (((==) tag) . pos) (Prelude.concat translations)
  let rightTrans = Prelude.map getText $ Prelude.concat (Prelude.map tr correctDefs)
  return rightTrans

fetchTranslations :: Text -> IO Value
fetchTranslations toTranslate = do
  r <- getWith opts url
  return $ (r ^.. responseBody . key "def") !! 0
  where
    url = "https://dictionary.yandex.net/api/v1/dicservice.json/lookup"
    apiKey = "dict.1.1.20170504T075234Z.863de6041969620d.502a3098d4cdae0ffe21f5ee040349b524dc3807"
    opts = defaults & param "key"  .~ [apiKey]
                    & param "text" .~ [toTranslate]
                    & param "lang" .~ ["en-fr"]
