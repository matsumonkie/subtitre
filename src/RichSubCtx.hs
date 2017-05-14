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
import Data.Aeson
import Data.ByteString.Lazy.Internal
import Control.Monad.IO.Class

createRichSubCtx :: RawSubCtx -> IO (Either [ParseError] RichSubCtx)
createRichSubCtx (SubCtx sequence timingCtx sentences) = do
  structuredSentences <- mapM runSpacy sentences :: IO [Text]
  let sentencesInfos = map parseSentenceStructure structuredSentences :: [Either ParseError SentenceInfos]
  return $ case (lefts sentencesInfos) of
        errors@(x:xs) -> Left errors
        _  -> Right $ subCtx (zip sentences (rights sentencesInfos))
  where
    subCtx = SubCtx sequence timingCtx

runSpacy :: Text -> IO Text
runSpacy sentence = do
  pack <$> spacy
  where
    spacy :: IO String
    spacy = readProcess "./client.py" ["-s", unpack sentence] []

serializeRichSubCtx :: RichSubCtx -> Text
serializeRichSubCtx richSubCtx =
  toStrict $ decodeUtf8 $ encode richSubCtx

serializeRichSubCtx' :: RichSubCtx -> ByteString
serializeRichSubCtx' richSubCtx =
  encode richSubCtx
