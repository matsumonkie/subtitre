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
import Control.Monad.Reader
import qualified Data.Text as T
import Config.App

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "composeSentence" $ do
    it "without any sentences" $ do
      composeSentence' Easy [a] `shouldReturn` "world (monde)"
    it "level to show is higher" $ do
      composeSentence' Hard [a] `shouldReturn` "world"
    it "shows punctuation correctly" $ do
      composeSentence' Easy [b] `shouldReturn` "Hello world (monde) !"
    it "" $ do
      composeSentence' Normal [c] `shouldReturn` "Dark Army told (dire) me stage (\233tape) two is (\234tre) ready."

composeSentence' :: Level -> [(Sentence, [WordInfos])] -> IO T.Text
composeSentence' level wis =
  let
    tp = undefined
    sc = undefined
  in composeSentence tp level translate sc wis :: IO T.Text

translate :: TP -> StaticConf -> WordInfos -> IO Translations
translate _ _ wi@(word, _, tag, _) =
  return $ if shouldBeTranslated tag then
             mkTranslations wi $ dictionary ! (T.toLower word)
           else
             mkTranslations wi []
  where
    shouldBeTranslated = (flip elem) [Verb, Noun, Adj, Sym, Punct, Propn, Pron, Conj, Adv]
    dictionary = fromList [ ("world", ["monde"])
                          , ("told", ["dire"])
                          , ("stage", ["étape"])
                          , ("is", ["être"])
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
