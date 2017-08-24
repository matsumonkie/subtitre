module AssetAsSet (
  assetPath
, assetAsSet
) where

import Common
import Prelude()

import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Type
import System.Directory

assetPath :: FilePath -> FilePath
assetPath file =
  "assets/" <> file

assetAsSet :: FilePath -> IO TextSet
assetAsSet file = do
  fileExist <- doesFileExist $ assetPath file
  if fileExist then
    HS.fromList <$> T.lines <$> readAsset file
  else
    return HS.empty
  where
    readAsset :: FilePath -> IO T.Text
    readAsset file =
      TIO.readFile $ assetPath file
