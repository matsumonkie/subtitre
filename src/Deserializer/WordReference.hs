{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Deserializer.WordReference (
  WRResponse(..)
, WRTerm(..)
, WRTranslation(..)
, allTranslations
) where

import Type
import Serializer
import Data.Text as T
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
import Data.Monoid
import Data.List
import Prelude as P

allTranslations :: WRResponse -> [WRTranslation]
allTranslations wrResponse =
  terms wrResponse >>= \x -> entries x ++ principalTranslations x ++ additionalTranslations x

data WRResponse =
  WRResponse { terms :: [WRTerm]
             } deriving (Show, Generic)

data WRTerm =
  WRTerm { entries :: [WRTranslation]
         , principalTranslations :: [WRTranslation]
         , additionalTranslations :: [WRTranslation]
         } deriving (Show)

data WRTranslation = WRTranslation { oTerm :: Text
                                   , oPos :: Tag
                                   , oSense :: Text
                                   , tTerm :: Text
                                   , tPos :: Tag
                                   , tSense :: Text
                                   } deriving (Show)

instance FromJSON WRResponse where
  parseJSON = withObject "WRResponse" $ \o -> do
    allTerms <- allWRTerms (HM.toList o)
    let terms = P.map snd $ sortOn fst allTerms
    return $ WRResponse { terms = terms }
    where
      allWRTerms :: [(Text, Value)] -> Parser [(Text, WRTerm)]
      allWRTerms entries = catMaybes <$> mapM parseWRTerm entries
      parseWRTerm :: (Text, Value) -> Parser (Maybe (Text, WRTerm))
      parseWRTerm (key, value) =
        if "term" `T.isPrefixOf` key then do
          v <- parseJSON value
          return $ Just (key, v)
        else
          return Nothing

instance FromJSON WRTerm where
  parseJSON = withObject "WRTerm" $ \o -> do
    entries <- o .:? "Entries" .!= HM.fromList []
    principalEntries <- o .:? "PrincipalTranslations" .!= HM.fromList []
    additionalEntries <- o .:? "AdditionalTranslations" .!= HM.fromList []
    entries <- mapM parse $ HM.toList entries
    principalTranslations <- mapM parse $ HM.toList principalEntries
    additionalTranslations <- mapM parse $ HM.toList additionalEntries

    return WRTerm { entries = getSortedTr entries
                  , principalTranslations = getSortedTr principalTranslations
                  , additionalTranslations = getSortedTr additionalTranslations }
    where
      getSortedTr x = P.map snd $ sortOn fst x
      parse :: (Text, Value) -> Parser (Int, WRTranslation)
      parse (index, value) = do
        v <- parseJSON (value)
        return (read $ unpack index, v)

instance FromJSON WRTranslation where
  parseJSON = withObject "WRTranslation" $ \o -> do
    originalTerm <- o .: "OriginalTerm"
    firstTranslation <- o .: "FirstTranslation"

    oTerm  <- originalTerm .: "term"
    oPos   <- originalTerm .: "POS" >>= wrTagParser
    oSense <- originalTerm .: "sense"
    tTerm  <- firstTranslation .: "term"
    tPos   <- firstTranslation .: "POS" >>= wrTagParser
    tSense <- firstTranslation .: "sense"
    return WRTranslation {..}

wrTagParser :: Value -> Parser Tag
wrTagParser =
  withText "word reference Tag" $ \t -> return $ case t of
    "adj"      -> Adj
    "loc adj"  -> Adj
    "n as adj" -> Adj
    "adj inv"  -> Adj
    "n"        -> Noun
    "n inv"    -> Noun
    "nf"       -> Noun
    "nm"       -> Noun
    "vtr"      -> Verb
    "vi"       -> Verb
    "v pron"   -> Verb
    "v expr"   -> Verb
    "loc v"    -> Verb
    _          -> Else
