module Main where

import Common
import Prelude()

import System.Environment
import Lib

main :: IO ()
main = do
  (file:fromLang:toLang:withLevel:_) <- getArgs
  runtimeConf <- getRuntimeConf file fromLang toLang (read withLevel :: Level)
  staticConf  <- getStaticConf
  translationsConf <- getTranslationsConf staticConf runtimeConf
  let config = Config runtimeConf staticConf translationsConf
  subtitle <- runExceptT (runReaderT run config)
  return ()
