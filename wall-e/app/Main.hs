module Main where

import Common
import Prelude()

import System.Environment
import qualified Data.Text as T
import Lib

getRuntimeConf :: FilePath -> String -> Level -> IO RuntimeConf
getRuntimeConf file to level = do
  levelSets <- getLevelSets
  return $
    RuntimeConf { translator = Lib.translate
                , levelSets = levelSets
                , dir = "/home/iori/temp"
                , file = file
                , to = to
                , levelToShow = level
                , logLevel = INFO
                , logFormatter = "[$time $loggername $prio] $msg"
                }

main :: IO ()
main = do
  (file:from:to:withLevel:_) <- getArgs
  runtimeConf <- getRuntimeConf file to (read withLevel)
  staticConf  <- getStaticConf
  let config = Config(runtimeConf, staticConf)
  subtitle <- runExceptT (runReaderT run config)
  pPrint subtitle
  return ()
