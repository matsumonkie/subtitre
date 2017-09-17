{-# LANGUAGE OverloadedStrings #-}

module Spacy.Spacify (
  spacify
) where

import Common
import Prelude()

import Config.App
import Control.Monad.Except
import Data.Either
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding
import Debug.Trace
import System.Process
import Text.Parsec hiding (parse)
import Type
import Redis.Connection
import Redis.Channel
import Redis.Handler
import Database.Redis
import Control.Concurrent
import qualified Data.Text.Encoding as E
import Spacy.Constants

spacify :: T.Text -> T.Text -> [RawSubCtx] -> Connection -> IO ()
spacify fromLang subId allRawSubCtx co = do
  let merged = mergeSubs allRawSubCtx
  undefined
  liftIO $ forkIO $ do
    runRedis co $ publish (spacifyChannel (E.encodeUtf8 fromLang) (E.encodeUtf8 subId)) (E.encodeUtf8 merged)
    return ()
  return ()

{-
i: ["hello \nworld", "it's me"]
o: "hello <$>world<*>it's me"
-}
mergeSubs :: [RawSubCtx] -> T.Text
mergeSubs allRawSubCtx =
  T.intercalate subSeparator $ map getSentence allRawSubCtx
  where
    getSentence :: RawSubCtx -> T.Text
    getSentence (SubCtx _ _ sentences) = T.intercalate sentenceSeparator sentences
