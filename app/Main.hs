{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import Type
import Parser
import qualified Data.Text as T hiding (map, head)
import qualified Data.Text.IO as IO
import Text.Pretty.Simple (pPrint)

import qualified Text.Parsec as Parsec

subtitleFile = "test.srt"

main :: IO ()
main = do
  fileContent <- readFile subtitleFile
  case parse fileContent of
    Right subCtx -> pPrint subCtx
    Left error -> pPrint error
  where
    parse = Parsec.parse subtitles "game of thrones"

--main = undefined
