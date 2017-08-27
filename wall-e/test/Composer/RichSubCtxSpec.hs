{-# LANGUAGE OverloadedStrings #-}

module Composer.RichSubCtxSpec (main, spec) where

import Text.Parsec
import Test.Hspec
import qualified Data.ByteString.Lazy as BSL
import Data.Functor
import Type
import Data.Monoid
import Data.Either
import RawSubParser
import Control.Monad.Trans.Except
import Composer.RichSubCtx
import LevelSet
import Data.HashMap.Strict hiding (map)
import Control.Monad.Reader
import Deserializer.WordReference
import qualified Data.Text as T
import Data.Aeson
import qualified Data.HashSet as HS
import Config.App

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "composeSentence" $ do
    it "without any sentences" $ do
      composeSentence' Easy [a] `shouldReturn` "<u>world</u> (<i>monde</i>)"
    it "level to show is higher" $ do
      composeSentence' Hard [a] `shouldReturn` "world"
    it "shows punctuation correctly" $ do
      composeSentence' Easy [b] `shouldReturn` "Hello <u>world</u> (<i>monde</i>) !"
    it "works with accent too" $ do
      composeSentence' Normal [c] `shouldReturn` "Dark Army told me <u>stage</u> (<i>sc\232ne</i>) two is ready."

composeSentence' :: Level -> [(Sentence, [WordInfos])] -> IO T.Text
composeSentence' level wis = do
  dic <- dictionary
  return $ composeSentence dic level dontTr wis

dontTr :: TextSet
dontTr = HS.empty

readResponse :: FilePath -> IO (Maybe Value)
readResponse file = do
  decode <$> (BSL.readFile $ path file)
  where
    path :: FilePath -> FilePath
    path file = "test/response/wr." <> file <> ".json"

dictionary :: IO Cache
dictionary = do
  rWorld <- readResponse "world"
  rTell <- readResponse "tell"
  rStage <- readResponse "stage"
  rIs <- readResponse "is"
  return $
    fromList [ ("world", rWorld)
             , ("told", rTell)
             , ("stage", rStage)
             , ("is", rIs)
             ]

a = ("world",
     [
       ("world", "world", Noun, Normal)
     ]
    )

b = ("Hello world!",
     [ ("Hello","hello", Else, Normal)
     , ("world","world", Noun, Normal)
     , ("!","!", Else, Normal)
     ]
    )

c = ("Dark Army told me stage two is ready.",
     [ ("Dark","dark", Adj, Easy)
     , ("Army","army", Propn, Easy)
     , ("told","tell", Verb, Hard)
     , ("me","-PRON-", Pron, Easy)
     , ("stage","stage", Noun, Hard)
     , ("two","two", Num, Easy)
     , ("is","be", Verb, Hard)
     , ("ready","ready", Adj, Easy)
     , (".",".", Punct, Easy)
     ]
    )
