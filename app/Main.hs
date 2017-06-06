{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Lib
import Type
import RawSubParser
import RichSubCtx
import Composer.RichSubCtx
import Text.Pretty.Simple (pPrint, pString)
import Data.Text.IO
import Data.Text hiding (map)
import Prelude hiding (readFile)
import Control.Monad.Trans.Except
import Text.Parsec
import qualified Data.Text.IO as TextIO
import qualified Data.ByteString.Lazy as LByteString (ByteString, toStrict)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy.IO as LTextIO (putStrLn)
import Control.Monad.IO.Class
import Data.Either
import Data.Monoid
import LevelSet
import Translator.Translate
import Control.Monad.Reader
import qualified Data.HashMap.Strict as HM
import Config.App

subtitleFile = "mini-sample.srt"
subtitleStructFile = "struct.srt"

saveToFile :: FilePath -> Text -> IO ()
saveToFile file content =
  TextIO.writeFile file content

main :: IO ()
main = do
  levelSets <- getLevelSets :: IO LevelSets
  staticConf <- getStaticConf
  let runtimeConf = RuntimeConf { translator = Translator.Translate.translate
                                , levelSets = levelSets
                                , levelToShow = Normal
                                , dir = "/home/iori/temp"
                                , file = "got.srt"
                                }
  let config = Config(runtimeConf, staticConf)
  runExceptT (runReaderT main' config)
  return ()

main' :: App ()
main' = do
  parsed <- parseSubtitlesOfFile
  riched <- createRichSubCtx parsed
  text   <- composeSubs riched
  outputFile <- asksR outputFile
  liftIO $ saveToFile outputFile text
  pPrint text


{-
parseSubtitles
  i: "1
      00:00:09,538 --> 00:00:12,373
      <i>( Gate rumbling,</i>
      <i>chains rattling )</i>"
  o: "seq 1
      timing 00:00:09,538 timing 00:00:12,373
      lines: ["<i>( Gate rumbling,</i>",
              "<i>chains rattling )</i>"]
-}

{-
createRichSubCtx
  i: "seq 1
      timing 00:00:09,538 timing 00:00:12,373
      lines: ["<i>( Gate rumbling,</i>",
              "<i>chains rattling )</i>"]
  o: "seq 1
      timing 00:00:09,538 timing 00:00:12,373
      lines: [(Gate, _, Noun, Easy), (rumbling,...)]
-}

{-
translate
  i: "seq 1
      timing 00:00:09,538 timing 00:00:12,373
      lines: [(Gate, _, Noun, Easy), (rumbling,...)]
  o: "seq 1
      timing 00:00:09,538 timing 00:00:12,373
      lines: [((Gate, _, Noun, Easy, ["porte"]), (rumbling,...)]
-}

{-
composeSubs
  i: "seq 1
      timing 00:00:09,538 timing 00:00:12,373
      lines: [((Gate, _, Noun, Easy, ["porte"]), (rumbling,...)]
  o: "seq 1
      00:00:09,538 -> 00:00:12,373
      Gate (porte), rumbling...
-}
