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
import Prelude hiding (lookup)
import Data.HashMap.Strict hiding (map)
import Control.Monad.Reader
import Data.Text
import Config.App

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "composeSentence" $ do
    it "without any sentences" $ do
      composeSentence' Easy [a] `shouldReturn` Right "world (monde)"
    it "level to show is higher" $ do
      composeSentence' Hard [a] `shouldReturn` Right "world"
    it "shows punctuation correctly" $ do
      composeSentence' Easy [b] `shouldReturn` Right "Hello world (monde) !"
    it "" $ do
      composeSentence' Normal [c] `shouldReturn` Right "Dark Army told (dire) me stage (\233tape) two is (\234tre) ready."

composeSentence' :: Level -> [(Sentence, SentenceInfos)] -> IO (Either [AppError] Text)
composeSentence' level wis =
  let
    runtimeConf = RuntimeConf { translator = translate
                              , levelSets = undefined
                              , levelToShow = level
                              , dir = "test/assets"
                              , file = undefined
                              }
    config = Config(runtimeConf, undefined)
  in runExceptT (runReaderT (composeSentence wis) config) :: IO (Either [AppError] Text)

translate :: WordInfos -> IO Translations
translate wi@(word, _, tag, _) =
  return $ if shouldBeTranslated tag then
             mkTranslations wi $ dictionary ! (toLower word)
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
