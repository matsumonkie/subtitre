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

subtitleFile = "mini-sample.srt"
subtitleStructFile = "struct.srt"

main :: IO ()
main = do
  res <- parseSubtitlesOfFile subtitleFile
  case res of
    Right subCtxts -> do
      let e = createRichSubCtx (subCtxts !! 0) :: ExceptT ParseError IO RichSubCtx
      let e' = mapM createRichSubCtx subCtxts :: ExceptT ParseError IO [RichSubCtx]
      e'' <- runExceptT e :: IO (Either ParseError RichSubCtx)
      case e'' of
        Left error ->
          pPrint "test"
        Right richSub ->
          pPrint "test"
    Left error ->
      pPrint error
--      let e = createRichSubCtx (subCtxts !! 0) :: ExceptT ParseError IO RichSubCtx
{-      let es = mapM createRichSubCtx subCtxts :: ExceptT ParseError IO [RichSubCtx]
      es' <- runExceptT es :: IO (Either ParseError [RichSubCtx])
      case (fmap compose es') of
        Left _ -> pPrint "nope"
        Right e -> do
          e' <- e
          pPrint e'
-}

putLazyByteStringLn :: LByteString.ByteString -> IO ()
putLazyByteStringLn = TextIO.putStrLn . lazyByteStringToText

lazyByteStringToString :: LByteString.ByteString -> String
lazyByteStringToString = unpack . lazyByteStringToText

lazyByteStringToText :: LByteString.ByteString -> Text
lazyByteStringToText = decodeUtf8 . LByteString.toStrict
