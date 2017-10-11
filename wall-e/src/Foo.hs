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
import Redis.Connection
import qualified Data.ByteString as BS


main :: IO ()
main = do
  putStrLn $ show $
    Common.foldl (\acc x -> acc + (T.length x)) 0 (["test"] :: [T.Text])
