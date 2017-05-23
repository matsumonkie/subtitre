{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator.Online (
  translate
, fetchTranslations
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
import Control.Exception
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.Either

data Def =
  Def { defText :: Text
      , defPos :: Tag
      , defTr :: [Tr]
      } deriving (Show, Generic)

data Tr =
  Tr { trText :: Text
     , trPos :: Tag
     } deriving (Show)

instance FromJSON Def where
  parseJSON = withObject "def" $ \o -> do
    defText <- o .: "text"
    defPos  <- o .: "pos"
    defTr  <- o .: "tr"
    return Def{..}

instance FromJSON Tr where
  parseJSON = withObject "tr" $ \o -> do
    trText <- o .: "text"
    trPos <- o .: "pos"
    return $ Tr{..}

shouldBeTranslated :: Tag -> Bool
shouldBeTranslated =
  (flip elem) [Verb, Noun, Adj, Sym, Punct, Propn, Pron, Conj, Adv]

translate :: WordInfos -> (Text -> IO (Maybe (Response ByteString))) -> IO Translations
translate wi@(word, lemma, tag, _) fetch
  | shouldBeTranslated tag = do
      response <- fetch toTranslate :: IO (Maybe (Response ByteString))
      let translations = (translationsBasedOnTag toTranslate tag) response
      res translations
  | otherwise = res []
  where
    res x = return $ mkTranslations wi x
    toTranslate = case tag of
      Verb -> lemma
      Noun -> lemma
      _ -> word

fetchTranslations :: Text -> IO (Maybe (Response ByteString))
fetchTranslations toTranslate = do
  catch (Just <$> (getWith opts url)) handler
  where
    handler :: HttpException -> IO (Maybe (Response ByteString))
    handler ex = return Nothing
    url = "https://dictionary.yandex.net/api/v1/dicservice.json/lookup"
    apiKey = "dict.1.1.20170504T075234Z.863de6041969620d.502a3098d4cdae0ffe21f5ee040349b524dc3807"
    opts = defaults & param "key"  .~ [apiKey]
                    & param "text" .~ [toTranslate]
                    & param "lang" .~ ["en-fr"]

translationsBasedOnTag :: Text -> Tag -> Maybe (Response ByteString) -> [Text]
translationsBasedOnTag toTranslate tag response = do
  let trs = toTrs response :: [Tr]
  let correctTrs = Prelude.filter (\x -> tag == trPos x) trs
  Prelude.map trText correctTrs

toTrs :: Maybe (Response ByteString) -> [Tr]
toTrs response =
  case response of
    Just r -> case (trs $ defs r) of
      t@(x:xs) -> t
      _ -> []
    Nothing -> []
  where
    trs :: [Def] -> [Tr]
    trs defs = Prelude.concat $ Prelude.map defTr defs
    rDefs :: Response ByteString -> Result [Def]
    rDefs r = (fromJSON $ toValue r)
    defs :: Response ByteString -> [Def]
    defs r = trace (show $ rDefs r) (successes $ rDefs r)
    toValue r = (r ^.. responseBody . key "def") !! 0 :: Value

successes :: Result [a] -> [a]
successes r =
  case r of
    Success e -> e
    Error _ -> []
