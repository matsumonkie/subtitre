{-# LANGUAGE OverloadedStrings #-}

module Deserializer.YandexSpec (main, spec) where

import Text.Parsec
import Test.Hspec
import Data.Functor
import Type
import RawSubParser
import Data.Monoid
import Data.Text hiding (map)
import Data.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.ByteString.Lazy hiding (elem)
import Prelude hiding (readFile)
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

readAsset :: FilePath -> IO (ByteString)
readAsset file = do
  readFile $ path file
  where
    path :: FilePath -> FilePath
    path file = "test/response/" <> file