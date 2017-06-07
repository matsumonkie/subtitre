{-# LANGUAGE OverloadedStrings #-}

module Deserializer.WordReferenceSpec (main, spec) where

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
import Deserializer.WordReference
import Data.Maybe
import Debug.Trace

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "WordReferenceSpec" $ do
    describe "from json" $ do
      it "works" $ do
        response <- decodeWR "wr.absorb.json"
        response `shouldSatisfy` isJust

decodeWR :: FilePath -> IO (Maybe WRResponse)
decodeWR file = decode <$> readAsset file

readAsset :: FilePath -> IO (ByteString)
readAsset file = do
  readFile $ path file
  where
    path :: FilePath -> FilePath
    path file = "test/response/" <> file
