module Lib
( module All
, run
, getRuntimeConf
, getTranslationsConf
) where

import Common
import Prelude()

import Composer.RichSubCtx as All
import Config.App as All
import Control.DeepSeq as All
import Control.Monad.Reader as All
import Control.Monad.Trans.Except as All
import Data.Monoid as All
import LevelSet as All
import Logger as All
import RawSubParser as All
import RichSubCtx as All
import Translator.Translate as All
import Type as All

import System.IO (stderr, stdout, Handle)
import System.Log as All (Priority(..))
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger hiding (logM, debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM)
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

setLogger :: App ()
setLogger = do
  logLevel <- asksR logLevel
  logFormatter <- asksR logFormatter
  myStreamHandler <- liftIO $ streamHandler stdout logLevel
  let myStreamHandler' = setFormatter myStreamHandler $ simpleLogFormatter logFormatter
  liftIO $ updateGlobalLogger rootLoggerName $ setLevel logLevel . setHandlers [myStreamHandler']

getRuntimeConf :: FilePath -> String -> Level -> IO RuntimeConf
getRuntimeConf file to level = do
  levelSets <- getLevelSets
  return $
    RuntimeConf { translator = translate
                , levelSets = levelSets
                , dir = "/home/iori/temp"
                , file = file
                , to = to
                , levelToShow = level
                , logLevel = INFO
                , logFormatter = "[$time $loggername $prio] $msg"
                }

getTranslationsConf = undefined

run :: App T.Text
run = do
  setLogger
  sc <- askS
  outputFile <- asksR outputFile
  parsed <- parseSubtitlesOfFile
  riched <- createRichSubCtx parsed
  text   <- composeSubs riched
  liftIO $ saveToFile outputFile text
  return text
  where
    saveToFile :: FilePath -> T.Text -> IO ()
    saveToFile file content =
      TIO.writeFile file content
