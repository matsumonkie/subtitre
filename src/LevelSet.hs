{-# LANGUAGE OverloadedStrings #-}

module LevelSet (
  whichLevel
, getLevelSets
) where

import Data.Text hiding (map)
import Prelude hiding (readFile, lines)
import Data.Monoid
import Data.HashSet
import Data.Text.IO
import GHC.IO.Handle hiding (hGetLine)
import System.IO hiding (readFile, hGetLine)
import Type
import Control.Applicative

assetAsSet :: FilePath -> IO LevelSet
assetAsSet file =
  fromList <$> lines <$> readAsset file
  where
    readAsset :: FilePath -> IO Text
    readAsset file =
      readFile $ "assets/" <> file

easyWords   = assetAsSet "500"
normalWords = assetAsSet "3000"
hardWords   = assetAsSet "10000"

getLevelSets :: IO LevelSets
getLevelSets = do
  e <- easyWords
  n <- normalWords
  h <- hardWords
  return $ LevelSets (e, n, h)

whichLevel :: Text -> LevelSets -> Level
whichLevel word (LevelSets(easySet, normalSet, hardSet))
  | word `member` easySet   = Easy
  | word `member` normalSet = Normal
  | word `member` hardSet   = Hard
  | otherwise = Unknown
