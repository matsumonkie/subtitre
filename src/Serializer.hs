{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Serializer (
) where

import Type
import Data.Text hiding (map, zipWith)
import Prelude hiding (Word, concat)
import Data.Aeson
import Data.Monoid

instance FromJSON (Tag) where
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
