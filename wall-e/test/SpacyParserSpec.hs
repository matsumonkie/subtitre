{-# LANGUAGE OverloadedStrings #-}

module SpacyParserSpec (main, spec) where

import Text.Parsec
import Test.Hspec
import Data.Functor
import Type
import Data.Monoid
import Data.Either
import RawSubParser
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import SpacyParser
import RichSubCtx
import LevelSet
import qualified  Data.HashSet as HS
import Config.App

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SentenceStructParser" $ do
    describe "parse" $ do
      context "simple sample" $ do
        it "simple line" $ do
          parseSpacySentence lSets ["I -PRON- PRON"] `shouldBe` Right [[("I", "-PRON-", Pron, Unknown)]]
        it "new line at end of file" $ do
          parseSpacySentence lSets ["I -PRON- PRON\n"] `shouldBe` Right [[("I", "-PRON-", Pron, Unknown)]]
        it "multiple argument" $ do
          length <$> parseSpacySentence lSets [multipleArg] `shouldBe` Right 4
        it "different level" $ do
          parseSpacySentence lSets ["worthless worthless ADJ"] `shouldBe` Right [[("worthless", "worthless", Adj, Unknown)]]
          parseSpacySentence lSets ["mighty mighty ADV"] `shouldBe` Right [[("mighty", "mighty", Adv, Unknown)]]

lSets :: LevelSets
lSets = LevelSets (HS.fromList [], HS.fromList [], HS.fromList [])

multipleArg = "I -PRON- PRON\n\
              \wanted want VERB\n\
              \ice ice NOUN\n\
              \creams cream NOUN"
