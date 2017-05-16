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
import Data.ByteString.Lazy hiding (elem)
import Text.Pretty.Simple (pPrint, pString)
import GHC.Exts
import Debug.Trace

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

shouldBeTranslated :: Tag -> Bool
shouldBeTranslated =
  (flip elem) [Adj, Adv, Conj, Noun, Pron, Punct, Sym, Verb]

translate :: WordInfos -> IO [Text]
translate wi@(word, lemma, tag)
  | shouldBeTranslated tag = do
      response <- fetchTranslations toTranslate :: IO (Response ByteString)
      let translations = (translationsBasedOnTag toTranslate tag) response
      return translations
  | otherwise = return []
  where
    toTranslate = case tag of
      Verb -> lemma
      _ -> word

fetchTranslations :: Text -> IO (Response ByteString)
fetchTranslations toTranslate = do
  getWith opts url
  where
    url = "https://dictionary.yandex.net/api/v1/dicservice.json/lookup"
    apiKey = "dict.1.1.20170504T075234Z.863de6041969620d.502a3098d4cdae0ffe21f5ee040349b524dc3807"
    opts = defaults & param "key"  .~ [apiKey]
                    & param "text" .~ [toTranslate]
                    & param "lang" .~ ["en-fr"]

translationsBasedOnTag :: Text -> Tag -> Response ByteString -> [Text]
translationsBasedOnTag toTranslate tag response = do
  let translations = toDefs response :: [Def]
  let correctDefs = Prelude.filter (((==) tag) . pos) translations
  Prelude.map getText $ Prelude.concat (Prelude.map tr correctDefs)

toDefs :: Response ByteString -> [Def]
toDefs response =
  Prelude.concat $ fromJSON parsed
  where
    parsed = (response ^.. responseBody . key "def") !! 0
