{-# LANGUAGE OverloadedStrings #-}

module RichSubCtx (
  structurize
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
import SentenceStructParser
import System.Process
import Text.Parsec hiding (parse)
import Type

sentenceSeparator = " <$> " :: T.Text
subSeparator      = " <*> " :: T.Text

structurize :: [RawSubCtx] -> App [RichSubCtx]
structurize allRawSubCtx = do
  levelSets <- asksR levelSets
  content <- liftIO $ runSpacy $ mergeSubs allRawSubCtx
  let unmerged    = unmergeSubs content
  let parsed      = map (parse levelSets) unmerged
  let richSubCtxs = mapM toRichSubCtx (zip allRawSubCtx parsed) :: Either [ParseError] [RichSubCtx]
  case richSubCtxs of
    Left pes -> do
      liftIO $ infoM "error"
--      liftIO $ infoM $ show content
--      liftIO $ infoM $ show unmerged !! 0
--      liftIO $ infoM $ show parsed !! 0
      liftIO $ infoM $ show pes
      throwError $ map AppError pes
    Right rs -> do
      return rs

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

{-
i: "hello\n <$> \nworld!\n <*> \nit's me"
o: [["hello", "world!"], ["it's me"]]
-}
unmergeSubs :: T.Text -> [[T.Text]]
unmergeSubs allSubs =
  map (T.splitOn ("\n" <> sentenceSeparator <> "\n")) (T.splitOn ("\n" <> subSeparator <> "\n") allSubs)

parse :: LevelSets -> [T.Text] -> Either [ParseError] [[WordInfos]]
parse levelSets sentence =
  case lefts parsed of
    [] -> Right $ rights parsed
    errors -> Left errors
  where
    parsed = map (parseSentenceStructure levelSets) sentence :: [Either ParseError [WordInfos]]

toRichSubCtx :: (RawSubCtx, Either [ParseError] [[WordInfos]])
             -> Either [ParseError] RichSubCtx
toRichSubCtx ((SubCtx sequence timingCtx sentences), parsed) =
  case parsed of
    Left errors -> Left errors
    Right sentencesInfos -> Right $ subCtx (zip sentences sentencesInfos)
  where
    subCtx = SubCtx sequence timingCtx

runSpacy :: T.Text -> IO T.Text
runSpacy sentence = do
  T.pack <$> spacy
  where
    spacy :: IO String
    spacy = readProcess "./spacy/client.py" ["-s", T.unpack sentence] []
