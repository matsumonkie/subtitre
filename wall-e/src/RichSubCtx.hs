{-# LANGUAGE OverloadedStrings #-}

module RichSubCtx (
  createRichSubCtxs
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
import Data.Either.Combinators

createRichSubCtxs :: [RawSubCtx] ->
                     [Either [ParseError] [[WordInfos]]]->
                     App [RichSubCtx]
createRichSubCtxs allRawSubCtx allWordsInfos = do
  let richSubCtxs = map toRichSubCtx (zip allRawSubCtx allWordsInfos) :: [Either [ParseError] RichSubCtx]
  return $ onlySuccess richSubCtxs

{-
  case richSubCtxs of
    Left pes -> do
      liftIO $ infoM "error while parsing to RichSubCtx"
      liftIO $ infoM $ show pes
      throwError $ map AppError pes
    Right rs -> do
      return rs
-}
toRichSubCtx :: (RawSubCtx, Either [ParseError] [[WordInfos]])
             -> Either [ParseError] RichSubCtx
toRichSubCtx ((SubCtx sequence timingCtx sentences), parsed) =
  case parsed of
    Left errors ->
      Left errors
    Right sentencesInfos ->
      Right $ subCtx (zip sentences sentencesInfos)
  where
    subCtx = SubCtx sequence timingCtx

onlySuccess :: [Either [ParseError] RichSubCtx] -> [RichSubCtx]
onlySuccess parsing =
  rights parsing
