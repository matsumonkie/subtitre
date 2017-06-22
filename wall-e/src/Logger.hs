module Logger (
  logM
, debugM
, infoM
, noticeM
, warningM
, errorM
, criticalM
, alertM
, emergencyM
) where

import Control.Monad.IO.Class
import qualified System.Log.Logger as HSLog

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
