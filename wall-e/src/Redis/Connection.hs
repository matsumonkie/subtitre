{-# LANGUAGE OverloadedStrings #-}

module Redis.Connection (
  redisCon
) where

import Common
import Prelude()

import Database.Redis

redisCon :: IO (Connection)
redisCon =
  checkedConnect defaultConnectInfo
