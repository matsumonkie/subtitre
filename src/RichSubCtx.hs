{-# LANGUAGE OverloadedStrings #-}

module RichSubCtx (
  createRichSubCtx
) where

import Type
import Text.Parsec.Combinator
import qualified Data.Text as T
import Text.Parsec
import System.Process
import GHC.IO.Handle
import Text.Pretty.Simple (pPrint)
import SentenceStructParser
import Data.Maybe

{-
createRichSubCtx :: RawSubCtx -> IO RichSubCtx
createRichSubCtx (SubCtx sequence timingCtx sentences) = do
  structuredSentences <- mapM runSpacy sentences :: IO [T.Text]
  let sentencesInfos = map parseSentenceStructure structuredSentences :: [Either ParseError SentenceInfos]
  let bar = zipp (zip sentencesInfos sentences) :: [Maybe (Sentence, SentenceInfos)]
  return $ SubCtx sequence timingCtx (catMaybes bar)
  where
    zipp :: [(Either ParseError SentenceInfos, T.Text)] -> [Maybe (Sentence, SentenceInfos)]
    zipp parsedSentences = map (\(a, b) -> either (const Nothing) (\y -> Just (b, y)) a) parsedSentences
-}

createRichSubCtx :: RawSubCtx -> IO RichSubCtx
createRichSubCtx (SubCtx sequence timingCtx sentences) = do
  structuredSentences <- mapM runSpacy sentences :: IO [T.Text]
  let sentencesInfos = map parseSentenceStructure structuredSentences :: [Either ParseError SentenceInfos]
  let bar = zipp (zip sentencesInfos sentences) :: [Maybe (Sentence, SentenceInfos)]
  return $ SubCtx sequence timingCtx (catMaybes bar)
  where
    zipp :: [(Either ParseError SentenceInfos, T.Text)] -> [Maybe (Sentence, SentenceInfos)]
    zipp parsedSentences = map (\(a, b) -> either (const Nothing) (\y -> Just (b, y)) a) parsedSentences

runSpacy :: T.Text -> IO T.Text
runSpacy sentence = do
  T.pack <$> spacy
  where
    spacy = readProcess "python" ["example.py", "-s", T.unpack sentence] []
