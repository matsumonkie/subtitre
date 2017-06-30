module Main where

import Common
import Prelude()

import System.Environment
import Lib

main :: IO ()
main = do
  (file:from:to:withLevel:_) <- getArgs
  runtimeConf <- getRuntimeConf file to (read withLevel)
  staticConf  <- getStaticConf
  translationsConf <- getTranslationsConf staticConf runtimeConf
  let config = Config runtimeConf staticConf translationsConf
  subtitle <- runExceptT (runReaderT run config)
--  pPrint subtitle
  return ()
