{-# LANGUAGE OverloadedStrings #-}

module Deserializer.WordReferenceSpec (main, spec) where

import Common
import Prelude()

import Test.Hspec
import Type
import qualified Data.ByteString.Lazy as BSL hiding (elem)
import Data.Aeson
import Deserializer.WordReference
import Data.Maybe

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "WordReferenceSpec" $ do
    describe "from json" $ do
      it "works" $ do
        response <- decodeWR "wr.absorb.json"
        response `shouldSatisfy` isJust
      it "display verb translations with right order" $ do
        response <- decodeWR "wr.snort.json"
        response `shouldSatisfy` isJust
        let wrResponse = fromJust response
        let tTerms = allTranslations wrResponse
        map tTerm tTerms `shouldBe` ["renâcler", "sniffer", "grognement", "rire", "grogner", "grogner"]
      it "shows translation with unusual structure" $ do
        response <- decodeWR "wr.whispering.json"
        response `shouldSatisfy` isJust
        let wrResponse = fromJust response
        let tTerms = allTranslations wrResponse
        map tTerm tTerms `shouldBe` ["murmure", "murmure", "chuchotement", "susurrant"]
      it "shows translation with multiple terms" $ do
        response <- decodeWR "wr.goat.json"
        response `shouldSatisfy` isJust
        let wrResponse = fromJust response
        let tTerms = allTranslations wrResponse
        map tTerm tTerms `shouldBe` ["chèvre", "pervers", "s'attaquer à", "attaquer"]

decodeWR :: FilePath -> IO (Maybe WRResponse)
decodeWR file = decode <$> readAsset file

readAsset :: FilePath -> IO (BSL.ByteString)
readAsset file = do
  BSL.readFile $ path file
  where
    path :: FilePath -> FilePath
    path file = "test/response/" <> file
