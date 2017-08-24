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

createRichSubCtxs :: [RawSubCtx] ->
                     [Either [ParseError] [[WordInfos]]]->
                     App [RichSubCtx]
createRichSubCtxs allRawSubCtx allWordsInfos = do
  let richSubCtxs = mapM toRichSubCtx (zip allRawSubCtx allWordsInfos) :: Either [ParseError] [RichSubCtx]
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

createRichSubCtx :: RawSubCtx
                    -> Either [ParseError] [[WordInfos]]
                    -> Either [ParseError] RichSubCtx
createRichSubCtx rawSubCtx eWordInfos = undefined

toRichSubCtx :: (RawSubCtx, Either [ParseError] [[WordInfos]])
             -> Either [ParseError] RichSubCtx
toRichSubCtx ((SubCtx sequence timingCtx sentences), parsed) =
  case parsed of
    Left errors -> Left errors
    Right sentencesInfos -> Right $ subCtx (zip sentences sentencesInfos)
  where
    subCtx = SubCtx sequence timingCtx
