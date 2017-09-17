{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}

module Redis.Handler (
  listenNewSubtitleRequest
, listenSpacified
, responseToText
) where

import Common
import Prelude()

import Config.App
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
import Control.Monad.Reader.Class
import Redis.Channel

responseToText :: Message -> T.Text
responseToText message =
  decodeUtf8 $ msgMessage message

listenNewSubtitleRequest :: (Message -> IO ()) -> IO ()
listenNewSubtitleRequest handler = do
  co <- checkedConnect defaultConnectInfo
  runRedis co $
    pubSub (subscribe subtitleChannelsPattern) $ \msg -> do
      handler msg
      return mempty

listenSpacified :: T.Text -> T.Text -> (Message -> IO PubSub) -> Redis ()
listenSpacified lang id = do
  let channel = spacifiedChannel (encodeUtf8 lang) (encodeUtf8 id)
  pubSub (subscribe [channel])
