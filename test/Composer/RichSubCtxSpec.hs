{-# LANGUAGE OverloadedStrings #-}

module Composer.RichSubCtxSpec (main, spec) where

import Text.Parsec
import Test.Hspec
import Data.Functor
import Type
import Data.Monoid
import Data.Either
import SentenceStructParser
import RawSubParser
import Control.Monad.Trans.Except
import Composer.RichSubCtx
import LevelSet
import Data.HashMap.Strict hiding (map)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "composeSentence" $ do
    it "without any sentences" $ do
      composeSentence Normal [a] `shouldReturn` "world (monde)"
      composeSentence Normal [b] `shouldReturn` "Hello world (monde) !"
      composeSentence Normal [c] `shouldReturn` "Dark Army told (dire) me stage (\233tape) two is (\234tre) ready."


a = ("world",
     [
       ("world", "world", Noun, Easy)
     ]
    )
b = ("Hello world!",
     [ ("Hello","hello", Else, Easy)
     , ("world","world", Noun, Easy)
     , ("!","!", Else, Easy)
     ]
    )
c = ("Dark Army told me stage two is ready.",
     [ ("Dark","dark", Adj, Easy)
     , ("Army","army", Propn, Easy)
     , ("told","tell", Verb, Easy)
     , ("me","-PRON-", Pron, Easy)
     , ("stage","stage", Noun, Easy)
     , ("two","two", Num, Easy)
     , ("is","be", Verb, Easy)
     , ("ready","ready", Adj, Easy)
     , (".",".", Punct, Easy)
     ]
    )
