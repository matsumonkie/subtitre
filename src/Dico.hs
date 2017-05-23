{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Dico (

) where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad
import Control.Monad.IO.Class
import Type
import Prelude hiding (words)
import Data.Text hiding (map, words)
import Control.Concurrent.Async
import Data.HashMap.Strict
import System.IO
import qualified Data.Text.IO as TextIO

type Dico = HashMap Text ()

a = fromList [ ("a",()) ] :: Dico

d :: Either FilePath Text
d = Left "10000"

e :: Either FilePath Text
e = Right "a"

class Content e where
  get :: e -> IO Text

instance Content (Either FilePath Text) where
  get r@(Left filePath) = TextIO.readFile filePath
  get r@(Right t) = return t

main = do
  TextIO.putStrLn undefined
