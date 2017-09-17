{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Common
import Prelude()
import Config.App

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import qualified Data.Text as T
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
import Redis.Channel
import Redis.PubSub
import Redis.Connection
import qualified Data.ByteString as BS
import Database.Redis

listenSpacified :: BS.ByteString -> BS.ByteString -> (Message -> IO PubSub) -> Redis ()
listenSpacified lang id = do
  let channel = spacifiedChannel lang id
  pubSub (subscribe [channel])

main :: IO ()
main = do
  e <- runExceptT (runReaderT run undefined)
  return ()

run :: App T.Text
run = do
  config <- ask
  co <- liftIO $ redisConnection
  liftIO $ runRedis co $ listenSpacified "en" "1" $ \msg -> do
    l <- runExceptT (runReaderT (run2 msg) config)
    return mempty
  undefined

run2 :: Message -> App T.Text
run2 response = undefined
