{-# LANGUAGE OverloadedStrings #-}

module Spacy.Constants (
  sentenceSeparator
, subSeparator
) where

import Common
import Prelude()

import qualified Data.Text as T

sentenceSeparator = " <$> " :: T.Text
subSeparator      = " <*> " :: T.Text
