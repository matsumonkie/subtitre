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
import Spacy
import SpacyParser

setLogger :: App ()
setLogger = do
  logLevel <- asksR logLevel
  logFormatter <- asksR logFormatter
  myStreamHandler <- liftIO $ streamHandler stdout logLevel
  let myStreamHandler' = setFormatter myStreamHandler $ simpleLogFormatter logFormatter
  liftIO $ updateGlobalLogger rootLoggerName $ setLevel logLevel . setHandlers [myStreamHandler']

getRuntimeConf :: FilePath -> T.Text -> T.Text -> Level -> IO RuntimeConf
getRuntimeConf file fromLang toLang level = do
  levelSets <- getLevelSets $ fromLang
  dontTranslate <- DT.dontTranslate fromLang toLang
  return $
    RuntimeConf { levelSets = levelSets
                , file = file
                , fromLang = fromLang
                , toLang = toLang
                , dontTranslate = dontTranslate
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
  liftIO $ infoM "parsing original file"
  rawSubCtxs   <- parseOriginal
  liftIO $ infoM "spacyfy"
  spacified    <- liftIO $ spacify rawSubCtxs
  liftIO $ infoM "parsing spacy"
  wordsInfos   <- parseSpacy spacified
  liftIO $ infoM "creating richSubCtx"
  richParsed <- createRichSubCtxs rawSubCtxs wordsInfos
  liftIO $ infoM "translating"
  cache        <- translate richParsed
  liftIO $ infoM "composing"
  composed     <- compose cache richParsed
  save composed
  return composed
  where
    save :: T.Text -> App ()
    save content = do
      outputFile <- asksR file
      let path = outputFile <> ".subtitre.srt"
      liftIO $ do
        infoM $ show path
        TIO.writeFile path content
