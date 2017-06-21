{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Concurrent.STM
import Control.Monad
import Data.HashMap.Strict as HM
import Data.Text
import System.Random
import Data.Maybe
import Control.Concurrent.Thread.Delay
import Control.Concurrent

type DB = TVar (HashMap Text Text)
type TakenCare = TVar [Text]
type Translations = TVar [Text]

delayS :: Integer -> IO ()
delayS n =
  delay $ n * 1000 * 1000

fetchOnline :: Text -> IO Text
fetchOnline key = do
  x <- randomIO :: IO Int
  delayS 0
  return $ pack $ show x

fetchFromDB :: DB -> Text -> IO (Maybe Text)
fetchFromDB fromDb key = do
  db <- readTVarIO fromDb
  return $ HM.lookup key db

writeOnDB :: DB -> DB -> (Text, Text) -> STM ()
writeOnDB fromDb fetched (key, value) = do
  db <- readTVar fromDb
  fetched' <- readTVar fetched
  let newDB = insert key value db
  writeTVar fromDb newDB
  writeTVar fetched newDB

translate :: TakenCare -> DB -> DB -> Translations -> Text -> IO Text
translate takenCare db fetched cache key = do
  cached <- readTVarIO cache
  if key `elem` cached then
    fetchFromDB db key >>= maybe (return "") return
  else do
    shouldBeTakenCare <- atomically $ process takenCare db key
    if shouldBeTakenCare then do
      value <- fetchOnline key
      atomically $ writeOnDB db fetched (key, value)
      return value
    else
      atomically $ fetchFromCache fetched key
  where
    fetchFromCache :: DB -> Text -> STM Text
    fetchFromCache cache key = do
      cache <- readTVar fetched
      case HM.lookup key cache of
        Just value -> return value
        Nothing -> retry

process :: TakenCare -> DB -> Text -> STM Bool
process takenCare db key = do
  needWork <- not <$> (isTakenCare takenCare key)
  if needWork then do
    setAsTakenCare takenCare key
    return True
  else
    return False

setAsTakenCare :: TakenCare -> Text -> STM ()
setAsTakenCare takenCare key = do
  tc <- readTVar takenCare
  writeTVar takenCare (key : tc)

isTakenCare :: TakenCare -> Text -> STM Bool
isTakenCare takenCare key = do
  tc <- readTVar takenCare
  if key `elem` tc then
    return True
  else
    return False

wordsToTranslate = ["car", "horse", "street"] :: [Text]

main :: IO [Text]
main = do
  fetched <- newTVarIO (HM.fromList([])) :: IO DB
  initialDB <- newTVarIO (HM.fromList([("horse", "cheval")])):: IO DB
  allreadyTranslated <- newTVarIO (["horse"]) :: IO (TVar [Text])
  takenCare <- newTVarIO ([]) :: IO (TVar [Text])
  let job = translate takenCare initialDB fetched allreadyTranslated  :: Text -> IO Text
  forConcurrently wordsToTranslate job


foo = undefined :: Int
bar = undefined :: Int
baz = undefined :: IO ()
mul = undefined :: IO Int

test :: IO Int
test = do
  let a = if True then
        foo
        else do
          bar
  return a
