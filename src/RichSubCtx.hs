{-# LANGUAGE OverloadedStrings #-}

module RichSubCtx (
  createRichSubCtx
, serializeRichSubCtx
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
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Debug.Trace

createRichSubCtx :: RawSubCtx -> ExceptT ParseError IO RichSubCtx
createRichSubCtx (SubCtx sequence timingCtx sentences) = do
  structuredSentences <- liftIO $ mapM runSpacy sentences
  let structuredSentence = structuredSentences !! 0
  let sentenceInfos = parseSentenceStructure structuredSentence :: Either ParseError SentenceInfos
  let ru = trace (show $ sentenceInfos) res sentenceInfos

  case ru of
    Right foo -> return $ SubCtx sequence timingCtx [foo]
    Left _ -> return $ SubCtx sequence timingCtx []
  where
    res :: Either ParseError SentenceInfos -> Either ParseError (Sentence, SentenceInfos)
    res si = si >>= (bar $ trace (T.unpack $ sentences !! 0) (sentences !! 0))

bar :: Sentence -> SentenceInfos -> Either ParseError (Sentence, SentenceInfos)
bar sentence sentenceInfos =
  Right (sentence, sentenceInfos)

runSpacy :: T.Text -> IO T.Text
runSpacy sentence = do
  T.pack <$> spacy
  where
    spacy :: IO String
    spacy = readProcess "./client.py" ["-s", T.unpack sentence] []

serializeRichSubCtx :: RichSubCtx -> T.Text
serializeRichSubCtx richSubCtx =
  undefined
