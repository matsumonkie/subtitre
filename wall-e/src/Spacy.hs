{-# LANGUAGE OverloadedStrings #-}

module Spacy (
  spacify
) where

import Common
import Prelude()

import Config.App
import Control.Monad.Except
import Data.Either
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding
import Debug.Trace
import System.Process
import Text.Parsec hiding (parse)
import Type

sentenceSeparator = " <$> " :: T.Text
subSeparator      = " <*> " :: T.Text

spacify :: [RawSubCtx] -> IO [[T.Text]]
spacify allRawSubCtx = do
  let merged = mergeSubs allRawSubCtx
  content <- runSpacy merged
  return $ unmergeSubs content

{-
i: ["hello \nworld", "it's me"]
o: "hello <$>world<*>it's me"
-}
mergeSubs :: [RawSubCtx] -> T.Text
mergeSubs allRawSubCtx =
  T.intercalate subSeparator $ map getSentence allRawSubCtx
  where
    getSentence :: RawSubCtx -> T.Text
    getSentence (SubCtx _ _ sentences) = T.intercalate sentenceSeparator sentences

runSpacy :: T.Text -> IO T.Text
runSpacy sentence = do
  T.pack <$> spacy
  where
    spacy :: IO String
    spacy = readProcess "./spacy/client.py" ["-s", T.unpack sentence] []

{-
i: "hello hello NOUN\n <$> \nworld world NOUN\n <*> \nyeah yeah NOUN"
o: [["hello hello NOUN", "world world NOUN"], ["yeah yeah NOUN"]]
-}
unmergeSubs :: T.Text -> [[T.Text]]
unmergeSubs allSubs =
  map (T.splitOn ("\n" <> sentenceSeparator <> "\n")) (T.splitOn ("\n" <> subSeparator <> "\n") allSubs)
