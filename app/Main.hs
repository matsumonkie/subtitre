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
import Translator.Translate
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM

subtitleFile = "mini-sample.srt"
subtitleStructFile = "struct.srt"

saveToFile :: FilePath -> Text -> IO ()
saveToFile file content =
  TextIO.writeFile file content

main :: IO ()
main = do
  levelSets <- getLevelSets :: IO LevelSets
  runReaderT main' (runtimeConf levelSets)
  where
    runtimeConf levelSets =
      RuntimeConf { translator = translate
                  , settings = HM.fromList []
                  , levelSets = levelSets
                  , levelToShow = Normal
                  , dir = "/home/iori/temp/"
                  , subFile = "8.srt"
                  }

main' = do
  conf <- ask
  res <- parseSubtitlesOfFile $ input conf
  case res of
    Right subCtxts -> do
      richSubCtxs <- createRichSubCtx subCtxts
      text <- composeSubs $ rights richSubCtxs
      liftIO $ saveToFile (output conf) text
      pPrint text
    Left e ->
      pPrint e
  where
    input conf = (dir conf) <> (subFile conf)
    output conf = (dir conf) <> "t" <> (subFile conf) :: FilePath
