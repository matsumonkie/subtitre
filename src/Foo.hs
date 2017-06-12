{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

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
