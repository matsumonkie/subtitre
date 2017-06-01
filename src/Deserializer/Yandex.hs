{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Deserializer.Yandex (
  YDef(..)
, YEntry(..)
, YTr(..)
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
import Data.ByteString.Lazy hiding (elem)
import Data.Traversable

data YDef =
  YDef { yEntries :: [YEntry]
       } deriving (Show)

data YEntry =
  YEntry { yEntryText :: Text
         , yEntryPos :: Tag
         , yEntryTr :: [YTr]
         } deriving (Show, Generic)

data YTr =
  YTr { yTrText :: Text
      , yTrPos :: Tag
      } deriving (Show)

instance FromJSON YDef where
  parseJSON = withObject "def" $ \o -> do
    yEntries <- o .: "def"
    return YDef{..}

instance FromJSON YEntry where
  parseJSON = withObject "entry" $ \o -> do
    yEntryText <- o .: "text"
    yEntryPos  <- o .: "pos"
    yEntryTr  <- o .: "tr"
    return YEntry{..}

instance FromJSON YTr where
  parseJSON = withObject "tr" $ \o -> do
    yTrText <- o .: "text"
    yTrPos <- o .: "pos"
    return $ YTr{..}

{-
toTrs :: Maybe (Response ByteString) -> [YTr]
toTrs response =
  case response of
    Just r -> case (trs $ defs r) of
      t@(x:xs) -> t
      _ -> []
    Nothing -> []
  where
    trs :: [YDef] -> [YTr]
    trs defs = Prelude.concat $ Prelude.map defTr defs
    rDefs :: Response ByteString -> Result [YDef]
    rDefs r = (fromJSON $ toValue r)
    defs :: Response ByteString -> [YDef]
    defs r = trace (show $ rDefs r) (successes $ rDefs r)
    toValue r = (r ^.. responseBody . key "def") !! 0 :: Value

successes :: Result [a] -> [a]
successes r =
  case r of
    Success e -> e
    Error _ -> []
-}
