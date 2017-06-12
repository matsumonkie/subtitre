{-# LANGUAGE OverloadedStrings #-}

module Logger (
  setLogger
, logM
, debugM
, infoM
, noticeM
, warningM
, errorM
, criticalM
, alertM
, emergencyM
) where

import Config.App
import qualified System.Log.Logger as HSLog
import System.Log.Handler.Syslog
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.IO (stderr, Handle)
import Control.Monad.IO.Class

loggerName = "SubT"

logM       = HSLog.logM       loggerName
debugM     = HSLog.debugM     loggerName
infoM      = HSLog.infoM      loggerName
noticeM    = HSLog.noticeM    loggerName
warningM   = HSLog.warningM   loggerName
errorM     = HSLog.errorM     loggerName
criticalM  = HSLog.criticalM  loggerName
alertM     = HSLog.alertM     loggerName
emergencyM = HSLog.emergencyM loggerName

setLogger :: App ()
setLogger = do
  logLevel <- asksR logLevel
  logFormatter <- asksR logFormatter
  myStreamHandler <- liftIO $ streamHandler stderr logLevel
  let myStreamHandler' = setFormatter myStreamHandler $ simpleLogFormatter logFormatter
  let log = HSLog.rootLoggerName
  liftIO $ HSLog.updateGlobalLogger log (HSLog.setHandlers [myStreamHandler'])
