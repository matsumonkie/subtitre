{-# LANGUAGE OverloadedStrings #-}

module Composer.RichSubCtxSpec (main, spec) where

import Text.Parsec
import Test.Hspec
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
import qualified Data.Text as T
import qualified Data.HashSet as HS
import Config.App

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "composeSentence" $ do
    it "without any sentences" $ do
      composeSentence' Easy [a] `shouldBe` "world (monde)"
    it "level to show is higher" $ do
      composeSentence' Hard [a] `shouldBe` "world"
    it "shows punctuation correctly" $ do
      composeSentence' Easy [b] `shouldBe` "Hello world (monde) !"
    it "" $ do
      composeSentence' Normal [c] `shouldBe` "Dark Army told (dire) me stage (\233tape) two is (\234tre) ready."

composeSentence' :: Level -> [(Sentence, [WordInfos])] -> T.Text
composeSentence' level wis =
  composeSentence dictionary level dontTr wis

dontTr :: TextSet
dontTr = HS.empty

dictionary :: Cache
dictionary = fromList [ ("world", Just "monde")
                      , ("told", Just "dire")
                      , ("stage", Just "étape")
                      , ("is", Just "être")
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
