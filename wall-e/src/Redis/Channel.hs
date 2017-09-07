{-# LANGUAGE OverloadedStrings #-}

module Redis.Channel (
  subtitleChannelsPattern
, spacyChannel
, spacifiedChannel
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

languages = ["fr", "en"]

subtitleChannelsPattern :: [BS.ByteString]
subtitleChannelsPattern =
  map subtitleChannelPattern languages

subtitleChannelPattern :: BS.ByteString -> BS.ByteString
subtitleChannelPattern lang =
  channelPattern "subtitle" lang

spacyChannel :: BS.ByteString -> BS.ByteString -> BS.ByteString
spacyChannel lang id =
  channel "spacy" lang id

spacifiedChannel :: BS.ByteString -> BS.ByteString -> BS.ByteString
spacifiedChannel lang id =
  channel "spacified" lang id

channelPattern :: BS.ByteString -> BS.ByteString -> BS.ByteString
channelPattern key lang =
  key <> ":" <> lang <> ":*"

channel :: BS.ByteString -> BS.ByteString -> BS.ByteString -> BS.ByteString
channel key lang id =
  key <> ":" <> lang <> ":" <> id

channelId :: Message -> T.Text
channelId msg =
  last $ T.splitOn ":" $ decodeUtf8 $ msgChannel msg
