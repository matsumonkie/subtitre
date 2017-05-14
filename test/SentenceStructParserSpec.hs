{-# LANGUAGE OverloadedStrings #-}

module SentenceStructParserSpec (main, spec) where

import Text.Parsec
import Test.Hspec
import Data.Functor
import Type
import Data.Monoid
import Data.Either
import SentenceStructParser
import RawSubParser
import Control.Monad.Trans.Except
import RichSubCtx

main :: IO ()
main = hspec spec

multipleArg = "I -PRON- PRON\n\
              \wanted want VERB\n\
              \ice ice NOUN\n\
              \creams cream NOUN"

satisfyIsRight :: Either [ParseError] RichSubCtx -> Expectation
satisfyIsRight parsed =
  parsed `shouldSatisfy` isRight

sherlock = "sherlock.srt"
mrRobot = "mr. robot.srt"
friends = "friends.srt"
house = "house.srt"
gameOfThrones = "game of thrones.srt"

parseFile :: FilePath -> IO (Either ParseError [RawSubCtx])
parseFile file =
  parseSubtitlesOfFile $ "test/assets/" <> file

spec :: Spec
spec = do
  describe "SentenceStructParser" $ do
    describe "parse" $ do
      context "simple sample" $ do
        it "simple line" $ do
          parseSentenceStructure "I -PRON- PRON" `shouldBe` (Right [("I", "-PRON-", Else)] :: Either Text.Parsec.ParseError SentenceInfos)
        it "new line at end of file" $ do
          parseSentenceStructure "I -PRON- PRON\n" `shouldBe` (Right [("I", "-PRON-", Else)] :: Either Text.Parsec.ParseError [WordInfos])
        it "multiple argument" $ do
          length <$> parseSentenceStructure multipleArg `shouldBe` Right 4

      context "real subtitles" $ do
        it "works" $ do
          eRawSubCtxs <- parseFile sherlock :: IO (Either ParseError [RawSubCtx])
          case eRawSubCtxs of
            Left _ -> "a" `shouldBe` "a"
            Right rawSubCtxs -> do
              eRichSubCtxs <- mapM createRichSubCtx (rawSubCtxs)
              lefts eRichSubCtxs `shouldBe` []

          {-
          parseFile mrRobot >>= satisfyIsRight
          parseFile house >>= satisfyIsRight
          parseFile gameOfThrones >>= satisfyIsRight
          parseFile friends >>= satisfyIsRight

-}
