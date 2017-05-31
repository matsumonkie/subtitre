{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Deserializer.WordReference (
  WRResponse(..)
, WRTerm(..)
, WRTranslation(..)
) where

import Type
import Serializer
import Data.Text
import Data.Maybe
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Data.Aeson
import Text.Pretty.Simple (pPrint, pString)
import GHC.Exts
import Debug.Trace
import Control.Exception
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.Either
import qualified Data.HashMap.Strict as HM
import Data.Traversable

data WRResponse =
  WRResponse { terms :: [WRTerm]
             } deriving (Show, Generic)

data WRTerm =
  WRTerm { principalTranslations :: [WRTranslation]
         , additionalTranslations :: [WRTranslation]
         } deriving (Show)

data WRTranslation = WRTranslation { oTerm :: Text
                                   , oPos :: Text
                                   , oSense :: Text
                                   , tTerm :: Text
                                   , tPos :: Text
                                   , tSense :: Text
                                   } deriving (Show)

instance FromJSON WRResponse where
  parseJSON = withObject "WRResponse" $ \o -> do
    terms <- allWRTerms (HM.toList o)
    return $ WRResponse {..}
    where
      allWRTerms :: [(Text, Value)] -> Parser [WRTerm]
      allWRTerms entries = catMaybes <$> mapM parseWRTerm entries
      parseWRTerm :: (Text, Value) -> Parser (Maybe WRTerm)
      parseWRTerm (key, value) =
        if "term" `isPrefixOf` key then
          Just <$> parseJSON value
        else
          return Nothing

instance FromJSON WRTerm where
  parseJSON = withObject "WRTerm" $ \o -> do
    principalEntries <- o .: "PrincipalTranslations"
    additionalEntries <- o .: "AdditionalTranslations"
    principalTranslations <- mapM parse (HM.toList principalEntries)
    additionalTranslations <- mapM parse (HM.toList additionalEntries)
    return WRTerm {..}
    where
      parse :: (Text, Value) -> Parser WRTranslation
      parse (_, value) = parseJSON value

instance FromJSON WRTranslation where
  parseJSON = withObject "WRTranslation" $ \o -> do
    originalTerm <- o .: "OriginalTerm"
    firstTranslation <- o .: "FirstTranslation"
    oTerm  <- originalTerm .: "term"
    oPos   <- originalTerm .: "POS"
    oSense <- originalTerm .: "sense"
    tTerm  <- firstTranslation .: "term"
    tPos   <- firstTranslation .: "POS"
    tSense <- firstTranslation .: "sense"
    return WRTranslation {..}
