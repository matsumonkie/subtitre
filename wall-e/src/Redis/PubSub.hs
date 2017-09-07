{-# LANGUAGE OverloadedStrings #-}

module Redis.PubSub (
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

connection :: IO Connection
connection = checkedConnect defaultConnectInfo
