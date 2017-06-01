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
import Control.Monad.Trans.Reader
import RichSubCtx
import LevelSet
import Data.HashMap.Strict hiding (map)
import Config.App

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "SentenceStructParser" $ do
    describe "parse" $ do
      context "simple sample" $ do
        it "simple line" $ do
          parseSentenceStructure lSets "I -PRON- PRON" `shouldBe` (Right [("I", "-PRON-", Pron, Unknown)] :: Either Text.Parsec.ParseError SentenceInfos)
        it "new line at end of file" $ do
          parseSentenceStructure lSets "I -PRON- PRON\n" `shouldBe` (Right [("I", "-PRON-", Pron, Unknown)] :: Either Text.Parsec.ParseError [WordInfos])
        it "multiple argument" $ do
          length <$> parseSentenceStructure lSets multipleArg `shouldBe` Right 4
        it "different level" $ do
          parseSentenceStructure lSets "worthless worthless ADJ" `shouldBe` (Right [("worthless", "worthless", Adj, Unknown)] :: Either Text.Parsec.ParseError [WordInfos])
          parseSentenceStructure lSets "mighty mighty ADV" `shouldBe` (Right [("mighty", "mighty", Adv, Unknown)] :: Either Text.Parsec.ParseError [WordInfos])

      context "real subtitles" $ do
        it "works" $ do
          parsingDoesntFail mrRobot
          parsingDoesntFail house
          parsingDoesntFail gameOfThrones
          parsingDoesntFail friends

lSets :: LevelSets
lSets = LevelSets (fromList [], fromList [], fromList [])

parsingDoesntFail :: FilePath -> Expectation
parsingDoesntFail file = do
  let runtimeConf = RuntimeConf { translator = undefined
                              , levelSets = lSets
                              , levelToShow = undefined
                              , dir = "test/assets"
                              , file = file
                              }
  let config = Config(runtimeConf, undefined)

  rawParsed <- runExceptT (runReaderT parseSubtitlesOfFile config) :: IO (Either [AppError] [RawSubCtx])
  let subs = either (const []) (id) rawParsed :: [RawSubCtx]
  let richSubs = createRichSubCtx subs :: App [RichSubCtx]
  richSubs <- runExceptT (runReaderT richSubs config)
  isRight richSubs `shouldBe` True

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
