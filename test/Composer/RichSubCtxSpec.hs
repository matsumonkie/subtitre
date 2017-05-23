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

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "composeSentence" $ do
    it "without any sentences" $ do
      composeSentence [a] `shouldReturn` "world (monde)"
--      composeSentence [b] `shouldReturn` "Hello world (monde) !"
--      composeSentence [c] `shouldReturn` "Dark Army told (dire) me stage (\233tape) two is (\234tre) ready."

a = ("world",
     [
       ("world", "world", Noun)
     ]
    )
b = ("Hello world!",
     [ ("Hello","hello", Else)
     , ("world","world", Noun)
     , ("!","!", Else)
     ]
    )
c = ("Dark Army told me stage two is ready.",
     [ ("Dark","dark", Adj)
     , ("Army","army", Propn)
     , ("told","tell", Verb)
     , ("me","-PRON-", Pron)
     , ("stage","stage", Noun)
     , ("two","two", Num)
     , ("is","be", Verb)
     , ("ready","ready", Adj)
     , (".",".", Punct)
     ]
    )
