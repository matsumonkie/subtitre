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

instance ToJSON (Tag) where
  toJSON tag =
    case tag of
      Verb -> "verb"
      Adj  -> "Adj"
      Else -> "Else"

instance ToJSON Level where
  toJSON tag =
    case tag of
      Easy -> "Easy"
      Normal  -> "Normal"
      Hard -> "Hard"
      Unknown -> "Unknown"

instance ToJSON WordInfos where
  toJSON (word, lemma, tag, level) =
    object [ "word"  .= word
           , "lemma" .= lemma
           , "tag"   .= tag
           , "level" .= level
           ]

instance ToJSON TimingCtx where
  toJSON (TimingCtx (Timing bh bm bs bms) (Timing eh em es ems)) =
    object [ "bh"  .= bh
           , "bm"  .= bm
           , "bs"  .= bs
           , "bms" .= bms
           , "eh"  .= eh
           , "em"  .= em
           , "es"  .= es
           , "ems" .= ems
           ]

instance ToJSON RichSubCtx where
  toJSON (SubCtx sequence timingCtx sentencesInfos) =
    object [ "sequence"  .= sequence
           , "timingCtx" .= timingCtx
           , "sentences" .= map sentenceCtx sentencesInfos
           ]
    where
      sentenceCtx (sentence, sentenceInfos) =
        object [ "raw" .= sentence
               , "pre" .= pre sentenceInfos
               , "wordsInfos" .= mapInd wordInfos sentenceInfos
               ]
      pre si = concat $ mapInd preprocess si

mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

wordInfos :: WordInfos -> Int -> Value
wordInfos wordInfo@(word, lemma, tag, level) idx =
  object [(pack $ show idx) .= wordInfo]

preprocess :: WordInfos -> Int -> Text
preprocess (word, lemma, tag, level) idx =
  if tag == Else then
    " " <> word
  else
    " %" <> (pack $ show idx)
