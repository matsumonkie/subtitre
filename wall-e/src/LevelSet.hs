module LevelSet (
  whichLevel
, getLevelSets
) where

import Common
import Prelude()

import qualified Data.HashSet as HS
import Type
import AssetAsSet
import qualified Data.Text as T

easyWords   lang = assetAsSet $ levelPath lang "500"
normalWords lang = assetAsSet $ levelPath lang "3000"
hardWords   lang = assetAsSet $ levelPath lang "10000"

levelPath lang file =
  "levels/" <> lang <> "/" <> file

getLevelSets :: Language -> IO LevelSets
getLevelSets lang = do
  e <- easyWords   $ T.unpack lang
  n <- normalWords $ T.unpack lang
  h <- hardWords   $ T.unpack lang
  return $ LevelSets (e, n, h)

whichLevel :: Word -> LevelSets -> Level
whichLevel word (LevelSets(easySet, normalSet, hardSet))
  | word `HS.member` easySet   = Easy
  | word `HS.member` normalSet = Normal
  | word `HS.member` hardSet   = Hard
  | otherwise = Unknown
