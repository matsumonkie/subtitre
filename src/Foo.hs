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

foo :: ExceptT String (WriterT String (StateT String (ReaderT String IO))) String
foo = do
  ask
  get
  put ""
  tell ""
  throwE ""
  return ""

data Bug = Bug
data Mistake = Mistake
data Error = Error Bug | Error2 Mistake

add1 :: ReaderT Int (ExceptT Error IO) Int
add1 = do
  initial <- ask
  return $ initial + 1

-- this compile
add2 :: Int -> ReaderT Int (ExceptT Error IO) Int
add2 e = do
  initial <- ask
  return $ e + 2 + initial

add3 :: Int -> ReaderT Int (ExceptT Error IO) Int
add3 e = do
  initial <- ask
  lift $ throwE (Error Bug)

bar :: ReaderT Int (ExceptT Error IO) Int
bar = do
--  let c = add1 >>= add2
  undefined

main = do
  e <- runExceptT (runReaderT bar 4)
  case e of
    Right e' -> putStrLn $ show e'
    Left _ -> putStrLn "nope"
