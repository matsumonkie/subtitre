{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Control.Monad.Writer.Class as W
import Data.Monoid
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Trans.Maybe
import Control.Concurrent.Async

data Bug = Bug
data Mistake = Mistake
data Error = Error Bug | Error2 Mistake

type App a = ReaderT [Int] IO a

add' :: Int -> Int -> IO (Async Int)
add' = do
  \x y -> async $ add x y

add :: Int -> Int -> IO Int
add y x = do
  return $ y + x

main' :: App ([Async Int])
main' = do
  env <- ask
  let a = mapM (add' 1) env :: IO ([Async Int])
  liftIO a

main = do
  e <- runReaderT main' [1,2,3,4]
  c <- mapM wait e
  putStrLn $ show c
