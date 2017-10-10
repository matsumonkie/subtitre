{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}

module Lib
( module All
, run
, getRuntimeConf
, getTranslationsConf
) where

import Common
import Prelude()

import qualified Data.ByteString as BS
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
import Redis.Connection
import Redis.Handler

import Control.Concurrent.STM
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr, stdout, Handle)
import System.Log as All (Priority(..))
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger hiding (logM, debugM, infoM, noticeM, warningM, errorM, criticalM, alertM, emergencyM)
import qualified DontTranslate as DT
import Spacy.Spacify
import Spacy.Parser
import Database.Redis
import Text.Parsec as Parsec
import Control.Concurrent
import Data.Text.Encoding

setLogger :: App ()
setLogger = do
  logLevel <- asksR logLevel
  logFormatter <- asksR logFormatter
  logFile <- asksS logFile
  myFileHandler <- liftIO $ fileHandler logFile logLevel
  myStreamHandler <- liftIO $ streamHandler stdout logLevel
  let myStreamHandler' = setFormatter myStreamHandler $ simpleLogFormatter logFormatter
  let myFileHandler' = setFormatter myFileHandler $ simpleLogFormatter logFormatter
  liftIO $ updateGlobalLogger rootLoggerName $ setLevel logLevel . setHandlers [myFileHandler', myStreamHandler']

getRuntimeConf :: FilePath -> T.Text -> T.Text -> Level -> T.Text -> IO RuntimeConf
getRuntimeConf file fromLang toLang level subId = do
  levelSets <- getLevelSets $ fromLang
  dontTranslate <- DT.dontTranslate fromLang toLang
  return $
    RuntimeConf { levelSets = levelSets
                , file = file
                , fromLang = fromLang
                , toLang = toLang
                , dontTranslate = dontTranslate
                , subId = subId
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

run :: App ()
run = do
  setLogger
  liftIO $ infoM "parsing original file"
  rawSubCtxs   <- parseOriginal
  liftIO $ infoM "spacyfy"
  co <- liftIO $ redisCon
  fromLang <- asksR fromLang
  subId <- asksR subId
  liftIO $ spacify fromLang subId rawSubCtxs co
  config <- ask :: App Config
  liftIO $ runRedis co $ listenSpacified fromLang subId $ \response -> do
    l <- runExceptT (runReaderT (runAfterSpacy (responseToText response) rawSubCtxs) config)
    return mempty
  return ()

runAfterSpacy :: T.Text -> [RawSubCtx] -> App ()
runAfterSpacy message rawSubCtxs = do
  liftIO $ infoM "parsing spacy"
  wordsInfos <- Spacy.Parser.parse message
  liftIO $ infoM "creating richSubCtx"
  richParsed <- createRichSubCtxs rawSubCtxs wordsInfos
  liftIO $ infoM "translating"
  cache        <- translate richParsed
  liftIO $ infoM "composing"
  composed     <- compose cache richParsed
  liftIO $ infoM "sending result back"
  subId <- asksR subId
  liftIO $ publishResult subId composed
  return ()
