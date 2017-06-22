module LevelSet (
  whichLevel
, getLevelSets
) where

import Common
import Prelude()

import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle hiding (hGetLine)
import System.IO hiding (readFile, hGetLine)
import Type

assetAsSet :: FilePath -> IO LevelSet
assetAsSet file =
  HS.fromList <$> T.lines <$> readAsset file
  where
    readAsset :: FilePath -> IO T.Text
    readAsset file =
      TIO.readFile $ "assets/" <> file

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
