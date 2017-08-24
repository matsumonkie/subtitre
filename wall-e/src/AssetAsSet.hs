module AssetAsSet (
  assetAsSet
) where

import Common
import Prelude()

import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Type

assetAsSet :: FilePath -> IO TextSet
assetAsSet file =
  HS.fromList <$> T.lines <$> readAsset file
  where
    readAsset :: FilePath -> IO T.Text
    readAsset file =
      TIO.readFile $ "assets/" <> file
