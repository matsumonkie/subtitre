module Config.RuntimeConf (
  RuntimeConf(..)
, inputFile
, outputFile
) where

import qualified Data.HashMap.Strict as HM
import Data.Monoid
import Type

data RuntimeConf =
  RuntimeConf { translator :: Translator
          , levelSets :: LevelSets
          , levelToShow :: Level
          , dir :: FilePath
          , file :: FilePath
          }

inputFile :: RuntimeConf -> FilePath
inputFile conf = (dir conf) <> "/" <> (file conf)

outputFile :: RuntimeConf -> FilePath
outputFile conf = (dir conf) <> "/t" <> (file conf)
