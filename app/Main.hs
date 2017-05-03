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

subtitleFile = "rawsub.srt"
subtitleStructFile = "struct.srt"

main :: IO ()
main = do
  content1 <- readFile subtitleFile
  case parseSubtitles content1 of
    Right subCtxts -> do
--      pPrint subCtxts
--      let e = createRichSubCtx (subCtxts !! 0) :: ExceptT ParseError IO RichSubCtx
      let es = mapM createRichSubCtx subCtxts :: ExceptT ParseError IO [RichSubCtx]
      es' <- runExceptT es :: IO (Either ParseError [RichSubCtx])
      pPrint $ fmap compose es'
--      e' <- runExceptT e :: IO (Either ParseError RichSubCtx)
--      pPrint $ fmap compose es'
--      case e' of
--        Right rich -> do
--          pPrint rich
--          LTextIO.putStrLn . pString . lazyByteStringToString $ serializeRichSubCtx' rich
--          pPrint $ compose [rich]
--        Left error -> pPrint error
--      pPrint $ composeSubtitles subCtxts

{-
      content2 <- readFile subtitleStructFile
      case parseSentenceStructure content2 of
        Right subCtxts -> do
          pPrint subCtxts
        Left error -> pPrint error
-}
    Left error -> pPrint error

putLazyByteStringLn :: LByteString.ByteString -> IO ()
putLazyByteStringLn = TextIO.putStrLn . lazyByteStringToText

lazyByteStringToString :: LByteString.ByteString -> String
lazyByteStringToString = unpack . lazyByteStringToText

lazyByteStringToText :: LByteString.ByteString -> Text
lazyByteStringToText = decodeUtf8 . LByteString.toStrict
