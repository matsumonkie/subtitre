{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import Common
import Prelude()
import Config.App

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Control.Monad.State
--import Control.Monad.Writer
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
import Data.Monoid

data Family = Family { _father :: User
                     , _mother :: User
                     , _kids :: [User]
                     } deriving Show

data User = User { _name :: String
                 , _job :: Maybe Job
                 } deriving (Show)

data Job = Writer | Actor | Teacher deriving Show

initialFamily = Family { _father = User { _name = "Robert", _job = Just Writer }
                       , _mother = User { _name = "Nadia", _job = Just Teacher }
                       , _kids = [ User { _name = "Emily", _job = Just Actor }
                                 , User { _name = "Nadia", _job = Nothing }
                                 ]
                       }

makeLenses ''Family
makeLenses ''User
makeLenses ''Job

main :: IO ()
main = do
--  putStrLn $ show initialFamily
  putStrLn $ show $ ((initialFamily^.kids) !! 0) ^. name <> "test" -- = "test"
