module Main where

import Common
import Prelude()

import qualified Data.Text as T
import Data.Text.Encoding
import Database.Redis
import Redis.Channel
import System.Environment
import Control.Concurrent
import Lib
import Redis.Handler

main :: IO ()
main = do
  staticConf <- getStaticConf
  listenNewSubtitleRequest $ \newRequest -> do
    forkIO $ do
      let filePath = T.unpack $ responseToText newRequest
      let (fromLang, toLang, subId) = channelInfos newRequest
      runtimeConf <- getRuntimeConf filePath fromLang toLang Hard subId
      translationsConf <- getTranslationsConf staticConf runtimeConf
      let config = Config runtimeConf staticConf translationsConf
      subtitle <- runExceptT (runReaderT run config)
      return ()
    return ()
  return ()
