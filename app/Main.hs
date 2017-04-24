{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import Type
import RawSubParser
import RichSubCtx
import Composer
import Text.Pretty.Simple (pPrint)
import Data.Text.IO
import Prelude hiding (readFile)
import Control.Monad.Trans.Except
import Text.Parsec

subtitleFile = "rawsub.srt"
subtitleStructFile = "struct.srt"

main :: IO ()
main = do
  content1 <- readFile subtitleFile
  case parseSubtitles content1 of
    Right subCtxts -> do
      pPrint subCtxts
      let e = createRichSubCtx (subCtxts !! 0) :: ExceptT ParseError IO RichSubCtx
      e' <- runExceptT e :: IO (Either ParseError RichSubCtx)
      case e' of
        Right rich -> pPrint rich
        Left error -> pPrint error
--      pPrint $ composeSubtitles subCtxts

{-
      content2 <- readFile subtitleStructFile
      case parseSentenceStructure content2 of
        Right subCtxts -> do
          pPrint subCtxts
        Left error -> pPrint error
-}
    Left error -> pPrint error
