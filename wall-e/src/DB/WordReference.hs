{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.WordReference (
  selectAll
, insertAll
) where

import Common
import Prelude()

import Type
import Config.App
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as BS hiding (elem, unpack, filter, map)
import Data.Aeson.Types
import Database.PostgreSQL.Simple

selectAll :: Config -> [Word] -> IO [(Word, Maybe Value)]
selectAll conf words = do
  con <- connectPostgreSQL $ dbConfig conf
  res <- query con q (toLang $ rc conf, In words) :: IO [(Word, Maybe Value)]
  close con
  return res
  where
    q = "SELECT word, response \
        \FROM translations t \
        \WHERE t.from_lang = 'en' \
        \AND t.to_lang = ? \
        \AND t.word in ? "

insertAll :: Config -> Word -> Language -> [(Word, Maybe Value)] -> IO ()
insertAll conf site toLang keysAndValues = do
  con <- connectPostgreSQL $ dbConfig conf
  now <- getCurrentTime
  executeMany con q $ map (param toLang now site) keysAndValues
  return ()
  where
    param :: String ->
             UTCTime ->
             Word ->
             (Word, Maybe Value) ->
             (Word, String, Word, Word, Maybe Value, UTCTime, UTCTime)
    param toLang now site (word, object) =
      ("en" :: Word,
       toLang,
       site,
       word,
       object,
       now,
       now)
    q = "INSERT INTO translations (\
        \\"from_lang\", \
        \\"to_lang\", \
        \\"site\", \
        \\"word\", \
        \\"response\", \
        \\"created_at\", \
        \\"updated_at\") \
        \ values (?, ?, ?, ?, ?, ?, ?) "

dbConfig :: Config -> BS.ByteString
dbConfig conf = BS.append (BS.append "dbname='" (Encoding.encodeUtf8 $ database $ sc conf)) "'"
