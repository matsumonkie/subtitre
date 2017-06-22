{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.WordReference (
  select
, insert
, available
) where

import Common
import Prelude()

import Type
import Config.App
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as BS hiding (elem, unpack, filter, map)
import Data.Aeson.Types
import Database.PostgreSQL.Simple

select :: StaticConf -> Word -> IO (Maybe Value)
select sc word = do
  con <- connectPostgreSQL config
  responses <- query con q (Only word) :: IO ([Only Value])
  close con
  return $ resToMaybe responses
  where
    config :: BS.ByteString
    config = BS.append (BS.append "dbname='" (Encoding.encodeUtf8 $ database sc)) "'"
    q = "SELECT response \
        \FROM wordreference w \
        \WHERE w.from = 'en' AND w.to = 'fr' AND w.word = ? \
        \LIMIT 1"
    resToMaybe :: [Only a] -> Maybe a
    resToMaybe responses =
      case responses of
        [Only i] -> Just i
        _ -> Nothing

available :: StaticConf -> IO [Word]
available sc = do
  con <- connectPostgreSQL config
  map unWrapOnly <$> query_ con q
  where
    config :: BS.ByteString
    config = BS.append (BS.append "dbname='" (Encoding.encodeUtf8 $ database sc)) "'"
    q = "SELECT DISTINCT word \
        \FROM wordreference w \
        \WHERE w.from = 'en' AND w.to = 'fr'"
    unWrapOnly :: Only a -> a
    unWrapOnly (Only a) = a

insert :: [(Word, Maybe Value)] -> IO ()
insert keysAndValues = do
  con <- connectPostgreSQL config
  now <- getCurrentTime
  executeMany con q $ map (param now) keysAndValues
  return ()
  where
    config = "dbname='subtitre_dev'"
    param :: UTCTime -> (Word, Maybe Value) -> (Word, Word, Word, Maybe Value, UTCTime, UTCTime)
    param now (word, object) =
      ("en" :: Word, "fr" :: Word, word, object, now, now)
    q = "INSERT INTO wordreference (\"from\", \"to\", \"word\", \"response\", \"created_at\", \"updated_at\") \
        \ values (?, ?, ?, ?, ?, ?) "
