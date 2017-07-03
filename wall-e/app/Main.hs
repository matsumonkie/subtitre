module Main where

import Common
import Prelude()

import System.Environment
import Lib

main :: IO ()
main = do
  (file:from:toLang:withLevel:_) <- getArgs
  runtimeConf <- getRuntimeConf file toLang (read withLevel :: Level)
  staticConf  <- getStaticConf
  translationsConf <- getTranslationsConf staticConf runtimeConf
  let config = Config runtimeConf staticConf translationsConf
  subtitle <- runExceptT (runReaderT run config)
--  pPrint subtitle
  return ()
