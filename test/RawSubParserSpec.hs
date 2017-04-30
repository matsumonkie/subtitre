{-# LANGUAGE OverloadedStrings #-}

module RawSubParserSpec (main, spec) where

import Text.Parsec
import Test.Hspec
import Data.Functor
import Type
import RawSubParser
import Data.Monoid
import Data.Text as T

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RawSubParserSpec" $ do
    describe "parse" $ do

      it "end with newline" $ do
        parseSubtitles (arg <> "\n") `shouldBe` Right [res ["(Hello)"]]

      it "end without newline" $ do
        parseSubtitles arg `shouldBe` Right [res ["(Hello)"]]

      it "multiple line" $ do
        parseSubtitles (arg <> "\nworld") `shouldBe` Right [res ["(Hello)", "world"]]

      it "multiple sub" $ do
        parseSubtitles (multipleSubs) `shouldBe` Right [res ["hello"], res ["world"]]

arg = "1\n\
      \00:00:26,722 --> 00:00:29,023\n\
      \(Hello)"

multipleSubs = "1\n\
      \00:00:26,722 --> 00:00:29,023\n\
      \hello\n\n\
      \1\n\
      \00:00:26,722 --> 00:00:29,023\n\
      \world"

res :: [T.Text] -> RawSubCtx
res text =
  SubCtx 1 (TimingCtx t1 t2) sentences
  where
    t1 = Timing 0 0 26 722
    t2 = Timing 0 0 29 23
    sentences = text
