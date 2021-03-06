{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Deserializer.Yandex (
  YDef(..)
, YEntry(..)
, YTr(..)
) where

import Common
import Prelude()

import Data.Aeson.Types
import Data.Text
import GHC.Generics
import Serializer
import Type

data Yandex

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
instance FromJSON Tag where
  parseJSON =
    withText "String" parse
    where
      parse x =
        return $ case x of
          "adj"   -> Adj
          "adv"   -> Adv
          "conj"  -> Conj
          "noun"  -> Noun
          "pron"  -> Pron
          "punct" -> Punct
          "sym"   -> Sym
          "verb"  -> Verb
          _       -> Else
-}
