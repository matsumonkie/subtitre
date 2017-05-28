{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import Type
import RawSubParser
import RichSubCtx
import Composer.RichSubCtx
import Text.Pretty.Simple (pPrint, pString)
import Data.Text.IO
import Data.Text hiding (map)
import Prelude hiding (readFile)
import Control.Monad.Trans.Except
import Text.Parsec
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString.Lazy as LByteString (ByteString, toStrict)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as LTextIO (putStrLn)
import Control.Monad.IO.Class
import Data.Either
import Data.Monoid
import LevelSet

subtitleFile = "mini-sample.srt"
subtitleStructFile = "struct.srt"

saveToFile :: FilePath -> Text -> IO ()
saveToFile file content =
  TextIO.writeFile file content

main :: IO ()
main = do
  let subSrt = "8.srt"
  levelSets <- getLevelSets :: IO LevelSets
  res <- parseSubtitlesOfFile $ "/home/iori/temp/" <> subSrt
  case res of
    Right subCtxts -> do
      richSubCtxs <- createRichSubCtx levelSets subCtxts :: IO [Either [ParseError] RichSubCtx]
      let foo = rights richSubCtxs :: [RichSubCtx]
      text <- compose Normal foo
      let output = "/home/iori/temp/t" <> subSrt :: FilePath
      saveToFile output text
      pPrint text
    Left e ->
      pPrint e

putLazyByteStringLn :: LByteString.ByteString -> IO ()
putLazyByteStringLn = TextIO.putStrLn . lazyByteStringToText

lazyByteStringToString :: LByteString.ByteString -> String
lazyByteStringToString = unpack . lazyByteStringToText

lazyByteStringToText :: LByteString.ByteString -> Text
lazyByteStringToText = decodeUtf8 . LByteString.toStrict
