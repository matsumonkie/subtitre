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
        parseSentenceStructure "-PRON- PRP" `shouldBe` (Right [("", "-PRON-", "PRP")] :: Either Text.Parsec.ParseError [WordInfos])

      it "new line at end of file" $ do
        parseSentenceStructure "-PRON- PRP\n" `shouldBe` (Right [("", "-PRON-", "PRP")] :: Either Text.Parsec.ParseError [WordInfos])

      it "multiple argument" $ do
        length <$> parseSentenceStructure multipleArg `shouldBe` Right 6

multipleArg = "-PRON- PRP\n\
              \want VBP\n\
              \to TO\n\
              \meet VB\n\
              \-PRON- PRP\n\
              \! ."
