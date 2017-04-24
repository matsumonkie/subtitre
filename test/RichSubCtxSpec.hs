{-# LANGUAGE OverloadedStrings #-}

module RichSubCtxSpec (main, spec) where

import Text.Parsec
import Test.Hspec
import Data.Functor
import Type
import RichSubCtx
--import Control.Monad.Trans.Except

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RichSubCtx" $ do
    describe "create" $ do
      it "simple sub ctx" $ do
        pending
--        (createRichSubCtx input) `shouldBe` output

input :: RawSubCtx
input =
  SubCtx sequence timingCtx sentence
  where
    sequence = 0
    timingCtx = TimingCtx (Timing 1 1 1 1) (Timing 1 1 1 1)
    sentence = [("hello !")]
{-
output :: ExceptT ParseError IO RichSubCtx
output =
  return $ SubCtx sequence timingCtx wordInfos
  where
    sequence = 0
    timingCtx = TimingCtx (Timing 1 1 1 1) (Timing 1 1 1 1)
    wordInfos = [("hello !", [("hello", "hello", "hello")])] :: [(Sentence, SentenceInfos)]
-}
