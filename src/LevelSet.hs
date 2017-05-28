{-# LANGUAGE OverloadedStrings #-}

module LevelSet (
  LevelSet
, LevelSets(..)
, whichLevel
, getLevelSets
) where

import Data.Text hiding (map)
import Prelude hiding (readFile, lines)
import Data.Monoid
import Data.HashMap.Strict hiding (map)
import Data.Text.IO
import GHC.IO.Handle hiding (hGetLine)
import System.IO hiding (readFile, hGetLine)
import Type

type LevelSet = HashMap Text ()
data LevelSets = LevelSets (LevelSet, LevelSet, LevelSet)

assetAsHash :: FilePath -> IO LevelSet
assetAsHash file =
  fromList <$> toPair <$> readAsset file
  where
    readAsset :: FilePath -> IO Text
    readAsset file =
      readFile $ "assets/" <> file
    toPair :: Text -> [(Text, ())]
    toPair text =
      map (\x -> (x, ())) $ lines text

easyWords   = assetAsHash "3000"
normalWords = assetAsHash "10000"
hardWords   = return $ fromList [] :: IO LevelSet

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
