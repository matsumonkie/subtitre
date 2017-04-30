{-# LANGUAGE OverloadedStrings #-}

module SentenceStructParserSpec (main, spec) where

import Text.Parsec
import Test.Hspec
import Data.Functor
import Type
import SentenceStructParser

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SentenceStructParser" $ do
    describe "parse" $ do
      it "simple line" $ do
        parseSentenceStructure "I -PRON- PRON" `shouldBe` (Right [("I", "-PRON-", Else)] :: Either Text.Parsec.ParseError SentenceInfos)

      it "new line at end of file" $ do
        parseSentenceStructure "I -PRON- PRON\n" `shouldBe` (Right [("I", "-PRON-", Else)] :: Either Text.Parsec.ParseError [WordInfos])

      it "multiple argument" $ do
        length <$> parseSentenceStructure multipleArg `shouldBe` Right 4

multipleArg = "I -PRON- PRON\n\
              \wanted want VERB\n\
              \ice ice NOUN\n\
              \creams cream NOUN"
