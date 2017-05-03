{-# LANGUAGE OverloadedStrings #-}

module Translator.Word (
  translate
) where

import Type
import Data.Text
import Data.HashMap.Strict
import Data.Maybe
import Prelude hiding (lookup)
import Control.Applicative

type Dictionary = HashMap Text Text

verbs :: Dictionary
verbs =
  fromList [ ("want", "vouloir")
           , ("meet", "rencontrer")
           ]

adjs :: Dictionary
adjs =
  fromList [("his", "sa/son")]

translate :: WordInfos -> Translation
translate wi@(word, lemma, tag) =
  case tag of
    Else -> (wi, Nothing)
    _    -> (wi, translation)
  where
    translation = lookup (snd dict) (fst dict)
    dict = case tag of
      Verb -> (verbs, lemma)
      Adj -> (adjs, word)
      _ -> (fromList[], "")
