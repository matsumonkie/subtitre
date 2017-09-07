module Main where

import Common
import Prelude()

import qualified Data.Text as T
import Data.Text.Encoding
import Database.Redis
import Redis.Channel
import System.Environment
import Lib
import Redis.Handler

main :: IO ()
main = do
  staticConf <- getStaticConf
  listenNewSubtitle $ \newRequest -> do
    let filePath = T.unpack $ decodeUtf8 $ msgMessage newRequest
    let (fromLang, toLang, id) = channelInfos newRequest
    runtimeConf <- getRuntimeConf filePath fromLang toLang Hard
    translationsConf <- getTranslationsConf staticConf runtimeConf
    let config = Config runtimeConf staticConf translationsConf
    subtitle <- runExceptT (runReaderT run config)
    return ()
