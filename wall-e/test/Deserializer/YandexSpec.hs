{-# LANGUAGE OverloadedStrings #-}

module Deserializer.YandexSpec (main, spec) where

import Test.Hspec
import Type
import Data.Monoid
import qualified Data.ByteString.Lazy as BSL hiding (elem)
import Data.Aeson
import Deserializer.Yandex
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Deserializer.Yandex" $ do
    describe "decode json" $ do
      it "works" $ do
        response <- decodeYandex "yandex.stirring.json"
        response `shouldSatisfy` isJust

decodeYandex :: FilePath -> IO (Maybe YDef)
decodeYandex file = decode <$> readAsset file

readAsset :: FilePath -> IO (BSL.ByteString)
readAsset file = do
  BSL.readFile $ path file
  where
    path :: FilePath -> FilePath
    path file = "test/response/" <> file
