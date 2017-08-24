module LevelSet (
  whichLevel
, getLevelSets
) where

import Common
import Prelude()

import qualified Data.HashSet as HS
import Type
import AssetAsSet

easyWords   lang = assetAsSet $ assetPath lang "500"
normalWords lang = assetAsSet $ assetPath lang "3000"
hardWords   lang = assetAsSet $ assetPath lang "10000"

assetPath lang file =
  "levels/" <> lang <> "/" <> file

getLevelSets :: Language -> IO LevelSets
getLevelSets lang = do
  e <- easyWords   lang
  n <- normalWords lang
  h <- hardWords   lang
  return $ LevelSets (e, n, h)

whichLevel :: Word -> LevelSets -> Level
whichLevel word (LevelSets(easySet, normalSet, hardSet))
  | word `HS.member` easySet   = Easy
  | word `HS.member` normalSet = Normal
  | word `HS.member` hardSet   = Hard
  | otherwise = Unknown
