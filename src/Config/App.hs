{-# LANGUAGE FlexibleContexts #-}
module Config.App (
  App
, AppError(..)
, Config(..)
, askR
) where

import Type
import Config.RuntimeConf
import Config.StaticConf
import Text.Parsec
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

data Config = Config (RuntimeConf, StaticConf)

data AppError = AppError ParseError deriving (Show, Eq)
type App a = ReaderT Config (ExceptT [AppError] IO) a

askR :: (Monad m) => (RuntimeConf -> a) -> ReaderT Config m a
askR f = do
  (Config(config, _)) <- ask
  return $ f config

askS :: (Monad m) => (StaticConf -> a) -> ReaderT Config m a
askS f = do
  (Config(_, config)) <- ask
  return $ f config
