{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader

import Control.Concurrent.Async
import Control.Concurrent.Thread.Delay
import Text.Pretty.Simple (pPrint, pString)

import Prelude hiding (Word)


type Sentence = String
type Word = String

f1 :: [[Sentence]] -> IO [[ [(Word, Word)] ]]
f1 subs = mapConcurrently (mapConcurrently f2) subs

f2 :: Sentence -> IO [(Word, Word)]
f2 sentence =
  mapConcurrently trProcess $ words sentence

trProcess :: Word -> IO (Word, Word)
trProcess = \word ->
  if needTranslation word then do
    translation <- translate word :: IO Word
    return (word, translation)
  else
    return (word, word)

translate :: Word -> IO Word
translate word = do
  putStrLn "in a thread"
  delay 1000000
  return $ case word of
    "car" -> "voiture"
    "street" -> "rue"
    "sure" -> "sur"
    _ -> ""

needTranslation :: String -> Bool
needTranslation word
  | shouldBeTranslated word = True
  | otherwise = False
  where
    shouldBeTranslated x = x `elem` ["car", "street", "sure"]

baz :: String -> IO (String, String)
baz =
  \x -> do
    putStrLn "creating threads"
    y <- getStr x
    return (x, y)

getStr :: String -> IO String
getStr _ = do
  putStrLn "in a thread"
  delay 1000000
  return "a"

--  async ::  IO a -> IO (Async a)
--  wait :: Async a -> IO a
main :: IO ()
main = do
  let c = [["the car is in the street", "oh yeah?"], ["sure !"]]
  e <- f1 c
  pPrint e


bez :: Reader String String
bez =
  biz

biz :: (MonadReader String m) => m String
biz = do
  e <- ask
  return e

boum :: (MonadIO m) => m String
boum = do
  e <- return "test"
  return e


boul :: [String] -> String
boul xs =
--  concat $ map ibe xs
  xs >>= ibe

ibe :: String -> String
ibe = undefined
