{-# LANGUAGE OverloadedStrings #-}

module RichSubCtx (
  createRichSubCtx
, serializeRichSubCtx
) where

import Type
import Text.Parsec.Combinator
import Data.Text hiding (map, zip)
import Text.Parsec
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

createRichSubCtx :: RawSubCtx -> ExceptT ParseError IO RichSubCtx
createRichSubCtx (SubCtx sequence timingCtx sentences) = do
  structuredSentences <- liftIO $ mapM runSpacy sentences
  let sentencesInfos = map parseSentenceStructure structuredSentences :: [Either ParseError SentenceInfos]
  return $ subCtx (rights $ map merge $ zip sentences sentencesInfos)
  where
    subCtx = SubCtx sequence timingCtx
    merge :: (Sentence, Either ParseError SentenceInfos) -> Either ParseError (Sentence, SentenceInfos)
    merge (s, e) = ((,) s) <$> e

runSpacy :: Text -> IO Text
runSpacy sentence = do
  pack <$> spacy
  where
    spacy :: IO String
    spacy = readProcess "./client.py" ["-s", unpack sentence] []

serializeRichSubCtx :: RichSubCtx -> Text
serializeRichSubCtx richSubCtx =
  undefined
