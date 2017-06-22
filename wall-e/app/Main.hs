module Main where

import Common
import Prelude()

import Lib

getRuntimeConf :: IO RuntimeConf
getRuntimeConf = do
  levelSets <- getLevelSets
  return $
    RuntimeConf { translator = Lib.translate
                , levelSets = levelSets
                , levelToShow = Hard
                , dir = "/home/iori/temp"
                , file = "robot.srt"
                , logLevel = INFO
                , logFormatter = "[$time $loggername $prio] $msg"
                }

main :: IO ()
main = do
  staticConf  <- getStaticConf
  runtimeConf <- getRuntimeConf
  let config = Config(runtimeConf, staticConf)
  subtitle <- runExceptT (runReaderT run config)
  pPrint subtitle
  return ()
