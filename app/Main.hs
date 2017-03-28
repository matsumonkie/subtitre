{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import Type
import Parser
import Composer
import Text.Pretty.Simple (pPrint)

subtitleFile = "test.srt"

main :: IO ()
main = do
  fileContent <- readFile subtitleFile
  case parseSubtitles fileContent of
    Right subCtxts -> do
      pPrint subCtxts
      pPrint $ composeSubtitles subCtxts
    Left error -> pPrint error
