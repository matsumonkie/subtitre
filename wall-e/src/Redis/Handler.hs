{-# LANGUAGE OverloadedStrings #-}

module Redis.Handler (
  listenNewSubtitle
) where

import Common
import Prelude()

import qualified Data.Text as T
import qualified Data.ByteString as BS hiding (elem, unpack, filter, map)
import Database.Redis
import Common
import Data.Monoid as All
import Control.Monad.IO.Class
import Data.Text.Encoding
import Control.Exception
import Control.Concurrent
import Control.Monad
import Redis.Channel
import Redis.PubSub

listenNewSubtitle :: IO ()
listenNewSubtitle = do
  co <- checkedConnect defaultConnectInfo
  runRedis co $
    pubSub (subscribe subtitleChannelsPattern) $ \msg -> do
      return mempty

main2 = do
  co <- checkedConnect defaultConnectInfo
  forkIO $ do
    runRedis co $ listenSpacified "en" "1"
  runRedis co $ publish "spacify:en:1" "just a test"

listenSpacified lang id = do
  let channel = spacifiedChannel lang id
  pubSub (subscribe [channel]) $ \msg -> do
--    putStrLn $ T.unpack $ decodeUtf8 $ msgMessage msg
    return $ unsubscribe [channel]

newSubtitleToHandle :: Message -> IO ()
newSubtitleToHandle msg = do
  threadDelay 10000
