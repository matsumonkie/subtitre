{-# LANGUAGE OverloadedStrings #-}

module RawSubParserSpec (main, spec) where

import qualified Text.Parsec as P
import Test.Hspec
import Data.Functor
import Type
import RawSubParser
import Data.Monoid
import Data.Text hiding (map)
import Data.Either
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Config.App
import qualified Text.HTML.TagSoup as TS

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "RawSubParserSpec" $ do
    describe "parse" $ do
      context "short sample" $ do
        it "end with newline" $ do
          parseSubtitles (arg <> "\n") `shouldBe` Right [res ["(Hello)"]]
        it "end without newline" $ do
          parseSubtitles arg `shouldBe` Right [res ["(Hello)"]]
        it "multiple line" $ do
          parseSubtitles (arg <> "\nworld") `shouldBe` Right [res ["(Hello)", "world"]]
        it "multiple sub" $ do
          parseSubtitles (multipleSubs) `shouldBe` Right [res ["hello"], res ["world"]]
      context "real subtitles" $ do
        it "parses real subtitles" $ do
          parseFile sherlock >>= satisfyIsRight
          parseFile mrRobot >>= satisfyIsRight
        it "with bom" $ do
          parseFile house >>= satisfyIsRight
        it "with ascii & CRLF" $ do
          parseFile gameOfThrones >>= satisfyIsRight
        it "with multiple back to back new lines" $ do
          parseFile theOffice >>= satisfyIsRight
      context "Latin-1" $ do
        it "parses weird characters" $ do
          parseFile friends >>= satisfyIsRight

sherlock = "sherlock.srt"
mrRobot = "mr. robot.srt"
friends = "friends.srt"
house = "house.srt"
theOffice = "theOffice.srt"
gameOfThrones = "game of thrones.srt"

parseFile :: FilePath -> IO (Either [AppError] [RawSubCtx])
parseFile file =
  let
    runtimeConf = RuntimeConf { levelSets = undefined
                              , levelToShow = undefined
                              , file = "test/assets/" <> file
                              , toLang = undefined
                              , logLevel = undefined
                              , logFormatter = undefined
                              }
    config = Config { rc = runtimeConf }
  in runExceptT (runReaderT parse config) :: IO (Either [AppError] [RawSubCtx])

arg = "1\n\
      \00:00:26,722 --> 00:00:29,023\n\
      \(Hello)"

multipleSubs = "1\n\
      \00:00:26,722 --> 00:00:29,023\n\
      \hello\n\n\
      \1\n\
      \00:00:26,722 --> 00:00:29,023\n\
      \world"

res :: [Text] -> RawSubCtx
res text =
  SubCtx 1 (TimingCtx t1 t2) sentences
  where
    t1 = Timing 0 0 26 722
    t2 = Timing 0 0 29 23
    sentences = text

satisfyIsRight :: Either [AppError] [RawSubCtx] -> Expectation
satisfyIsRight parsed =
  parsed `shouldSatisfy` isRight
