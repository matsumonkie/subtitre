module Main where

import Common
import Prelude()

import Lib
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
  runExceptT (runReaderT main' config)
  return ()

main' :: App ()
main' = do
  setLogger
  sc <- askS
  s <- ask
  outputFile <- asksR outputFile
  liftIO $ infoM $ s `deepseq` "Config is fine"
  parsed <- parseSubtitlesOfFile
  riched <- createRichSubCtx parsed
  text   <- composeSubs riched
  liftIO $ saveToFile outputFile text
  pPrint text
  where
    saveToFile :: FilePath -> T.Text -> IO ()
    saveToFile file content =
      TIO.writeFile file content
