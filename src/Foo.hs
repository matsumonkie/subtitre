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

foo :: ExceptT String (WriterT String (StateT String (ReaderT String IO))) String
foo = do
  ask
  get
  put ""
  tell ""
  throwE ""
  return ""

addTest :: Reader String String
addTest = do
  str' <- ask
  return $ str' ++ ":test"

baz :: Reader String (IO String)
baz = do
  b <- addTest :: Reader String String
  return $ bee b

bee :: String -> IO String
bee b = return $ b ++ ":io"

main = do
--  putStrLn $ show $ runReader addTest "someEnv"
  e <- runReader baz "someEnv2"
  putStrLn $ show $ e
