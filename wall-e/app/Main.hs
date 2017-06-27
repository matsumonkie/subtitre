module Main where

import Common
import Prelude()

import System.Environment
import qualified Data.Text as T
import Lib

main :: IO ()
main = do
  (file:from:to:withLevel:_) <- getArgs
  runtimeConf <- getRuntimeConf file to (read withLevel)
  staticConf  <- getStaticConf
  let config = Config runtimeConf staticConf getTranslationsConf
  subtitle <- runExceptT (runReaderT run config)
  pPrint subtitle
  return ()
