{-# LANGUAGE OverloadedStrings #-}

module RichSubCtx (
  createRichSubCtx
, serializeRichSubCtx
, serializeRichSubCtx'
) where

import Type
import Serializer
import Text.Parsec.Combinator
import Data.Text hiding (map, zip)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding
import Text.Parsec hiding (parse)
import System.Process
import GHC.IO.Handle
import Text.Pretty.Simple (pPrint)
import SentenceStructParser
import Data.Maybe
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Debug.Trace
import Data.Either
import Data.Aeson
import Data.ByteString.Lazy.Internal
import Control.Monad.IO.Class
import Data.Monoid
import LevelSet

sentenceSeparator = " <$> " :: Text
subSeparator      = " <*> " :: Text

createRichSubCtx :: LevelSets -> [RawSubCtx] -> IO [Either [ParseError] RichSubCtx]
createRichSubCtx levelSets allRawSubCtx = do
  content <- runSpacy $ mergeSubs allRawSubCtx
  let unmerged    = unmergeSubs content
  let parsed      = map (parse levelSets) unmerged
  let richSubCtxs = map toRichSubCtx (zip allRawSubCtx parsed)
  return richSubCtxs

mergeSubs :: [RawSubCtx] -> Text
mergeSubs allRawSubCtx =
  intercalate subSeparator $ map getSentence allRawSubCtx
  where
    getSentence :: RawSubCtx -> Text
    getSentence (SubCtx _ _ sentences) = intercalate sentenceSeparator sentences

{-
input = "hello\n <$> \nworld!\n <*> \nit's me"
output = [["hello", "world!"], ["it's me"]]
-}
unmergeSubs :: Text -> [[Text]]
unmergeSubs allSubs =
  map (splitOn ("\n" <> sentenceSeparator <> "\n")) (splitOn ("\n" <> subSeparator <> "\n") allSubs)

parse :: LevelSets -> [Text] -> Either [ParseError] [SentenceInfos]
parse levelSets sentence =
  case lefts parsed of
    [] -> Right $ rights parsed
    errors -> Left errors
  where
    parsed = map (parseSentenceStructure levelSets) sentence :: [Either ParseError SentenceInfos]

toRichSubCtx :: (RawSubCtx, Either [ParseError] [SentenceInfos])
             -> Either [ParseError] RichSubCtx
toRichSubCtx ((SubCtx sequence timingCtx sentences), parsed) =
  case parsed of
    Left errors -> Left errors
    Right sentencesInfos -> Right $ subCtx (zip sentences sentencesInfos)
  where
    subCtx = SubCtx sequence timingCtx

runSpacy :: Text -> IO Text
runSpacy sentence = do
  pack <$> spacy
  where
    spacy :: IO String
    spacy = readProcess "./spacy/client.py" ["-s", unpack sentence] []

serializeRichSubCtx :: RichSubCtx -> Text
serializeRichSubCtx richSubCtx =
  toStrict $ decodeUtf8 $ encode richSubCtx

serializeRichSubCtx' :: RichSubCtx -> ByteString
serializeRichSubCtx' richSubCtx =
  encode richSubCtx
