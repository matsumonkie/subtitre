{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Composer.RichSubCtx
import Config.App
import Control.DeepSeq
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import qualified Data.ByteString.Lazy as LByteString (ByteString, toStrict)
import Data.Either
import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Data.Text hiding (map)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO
import qualified Data.Text.IO as TextIO
import qualified Data.Text.Lazy.IO as LTextIO (putStrLn)
import LevelSet
import Lib
import qualified Logger as L
import Prelude hiding (readFile)
import RawSubParser
import RichSubCtx
import qualified System.Log.Logger as HSLogger
import Text.Parsec
import Text.Pretty.Simple (pPrint, pString)
import Translator.Translate
import Type

main :: IO ()
main = do
  levelSets <- getLevelSets :: IO LevelSets
  staticConf <- getStaticConf
  let runtimeConf = RuntimeConf { translator = Translator.Translate.translate
                                , levelSets = levelSets
                                , levelToShow = Easy
                                , dir = "/home/iori/temp"
                                , file = "foo.srt"
                                , logLevel = HSLogger.INFO
                                , logFormatter = "[$time $loggername $prio] $msg"
                                }
  let config = Config(runtimeConf, staticConf)
  runExceptT (runReaderT main' config)
  return ()

main' :: App ()
main' = do
  L.setLogger
  sc <- askS
  s <- ask
  outputFile <- asksR outputFile
  liftIO $ L.infoM $ s `deepseq` "Config is fine"
  parsed <- parseSubtitlesOfFile
  riched <- createRichSubCtx parsed
  text   <- composeSubs riched
  liftIO $ saveToFile outputFile text
  pPrint text
  where
    saveToFile :: FilePath -> Text -> IO ()
    saveToFile file content =
      TextIO.writeFile file content
