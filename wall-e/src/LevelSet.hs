module LevelSet (
  whichLevel
, getLevelSets
) where

import Common
import Prelude()

import qualified Data.HashSet as HS
import Type
import AssetAsSet

easyWords   = assetAsSet "500"
normalWords = assetAsSet "3000"
hardWords   = assetAsSet "10000"

getLevelSets :: IO LevelSets
getLevelSets = do
  e <- easyWords
  n <- normalWords
  h <- hardWords
  return $ LevelSets (e, n, h)

whichLevel :: Word -> LevelSets -> Level
whichLevel word (LevelSets(easySet, normalSet, hardSet))
  | word `HS.member` easySet   = Easy
  | word `HS.member` normalSet = Normal
  | word `HS.member` hardSet   = Hard
  | otherwise = Unknown
