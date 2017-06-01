{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Translator.Offline (
  translate
) where

import Type
import Data.Text
import Data.HashMap.Strict
import Data.Maybe
import Prelude hiding (lookup)
import Control.Applicative
import Data.Functor.Identity
import Text.Pretty.Simple (pPrint, pString)
import Config.App

type Dictionary = HashMap Text Text

verbs :: Dictionary
verbs =
  fromList [ ("want", "vouloir")
           , ("meet", "rencontrer")
           ]

adjs :: Dictionary
adjs =
  fromList [("his", "sa/son")]

translate :: WordInfos -> App Translations
translate wi@(word, lemma, tag, level) =
  return $ mkTranslations wi $ case tag of
    Else -> []
    _    -> maybe [] (\x -> [x]) translation
  where
    translation = lookup (snd dict) (fst dict) :: Maybe Text
    dict = case tag of
      Verb -> (verbs, lemma)
      Adj -> (adjs, word)
      _ -> (fromList[], "")
