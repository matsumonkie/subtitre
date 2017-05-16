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
          parseSentenceStructure "I -PRON- PRON" `shouldBe` (Right [("I", "-PRON-", Pron)] :: Either Text.Parsec.ParseError SentenceInfos)
        it "new line at end of file" $ do
          parseSentenceStructure "I -PRON- PRON\n" `shouldBe` (Right [("I", "-PRON-", Pron)] :: Either Text.Parsec.ParseError [WordInfos])
        it "multiple argument" $ do
          length <$> parseSentenceStructure multipleArg `shouldBe` Right 4

      context "real subtitles" $ do
        it "works" $ do
          parsingDoesntFail mrRobot
          parsingDoesntFail house
          parsingDoesntFail gameOfThrones
          parsingDoesntFail friends

parsingDoesntFail :: FilePath -> Expectation
parsingDoesntFail file = do
  rawParsed <- parseFile file :: IO (Either ParseError [RawSubCtx])
  let subs = either (const []) (id) rawParsed :: [RawSubCtx]
  richSubs <- createRichSubCtx subs
  case (richSubs) of
    Right (SubCtx _ _  ((s, si):ss)   ):r -> putStrLn $ show si

  lefts richSubs `shouldBe` []
