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

import Control.Concurrent.STM
import qualified DB.WordReference as DB
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr, stdout, Handle)
import System.Log as All (Priority(..))
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger hiding (logM, debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM)

setLogger :: App ()
setLogger = do
  logLevel <- asksR logLevel
  logFormatter <- asksR logFormatter
  myStreamHandler <- liftIO $ streamHandler stdout logLevel
  let myStreamHandler' = setFormatter myStreamHandler $ simpleLogFormatter logFormatter
  liftIO $ updateGlobalLogger rootLoggerName $ setLevel logLevel . setHandlers [myStreamHandler']

getRuntimeConf :: FilePath -> String -> Level -> IO RuntimeConf
getRuntimeConf file toLang level = do
  levelSets <- getLevelSets
  return $
    RuntimeConf { levelSets = levelSets
                , file = file
                , toLang = toLang
                , levelToShow = level
                , logLevel = INFO
                , logFormatter = "[$time $loggername $prio] $msg"
                }

getTranslationsConf :: StaticConf -> RuntimeConf -> IO TranslationsConf
getTranslationsConf sc rc = do
  responsesToSave <- newTVarIO $ HM.fromList([])
  currentNbOfOnlineRequest <- newTVarIO 0
  return $
    TranslationsConf { responsesToSave           = responsesToSave
                     , currentNbOfOnlineRequest  = currentNbOfOnlineRequest
                     }

run :: App T.Text
run = do
  setLogger
  parsed       <- parse
  structurized <- structurize parsed
  cache        <- translate structurized
  composed     <- compose cache structurized
  save composed
  return composed
  where
    save :: T.Text -> App ()
    save content = do
      outputFile <- asksR file
      liftIO $ TIO.writeFile (outputFile <> ".output") content
