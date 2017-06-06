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
import Config.App

subtitleFile = "mini-sample.srt"
subtitleStructFile = "struct.srt"

saveToFile :: FilePath -> Text -> IO ()
saveToFile file content =
  TextIO.writeFile file content

main :: IO ()
main = do
  levelSets <- getLevelSets :: IO LevelSets
  staticConf <- getStaticConf
  let runtimeConf = RuntimeConf { translator = Translator.Translate.translate
                                , levelSets = levelSets
                                , levelToShow = Normal
                                , dir = "/home/iori/temp"
                                , file = "8.srt"
                                }
  let config = Config(runtimeConf, staticConf)
  runExceptT (runReaderT main' config)
  return ()

main' :: App ()
main' = do
  outputFile <- askR outputFile
  parsed <- parseSubtitlesOfFile
  riched <- createRichSubCtx parsed
  text   <- composeSubs riched
  liftIO $ saveToFile outputFile text
  pPrint text
