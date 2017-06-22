{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module DB.WordReference (
  select
, insert
, available
) where

import Type
import Data.Text
import qualified Translator.Offline as Offline
import Control.Applicative
import qualified Translator.Strategy.Yandex as Yandex
import qualified Translator.Strategy.WordReference as WordReference
import Config.App
import Data.Monoid
import qualified Logger as L
import Debug.Trace
import Text.Pretty.Simple (pPrint, pString)
import Prelude as P
import Control.Concurrent.STM
import qualified Data.Text.Encoding as Encoding
import qualified Data.ByteString as B hiding (elem, unpack, filter, map)
import Data.Aeson.Types
import Database.PostgreSQL.Simple
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.ByteString.Lazy hiding (elem, unpack, filter, map)
import Network.Wreq
import Type
import Serializer
import Data.Text hiding (filter, map)
import Data.Maybe
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy hiding (elem, unpack, filter, map)
import Text.Pretty.Simple (pPrint, pString)
import GHC.Exts
import Debug.Trace
import Control.Exception
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.Either
import Deserializer.Yandex
import Config.App
import Data.Monoid
import Control.Monad.IO.Class
import Deserializer.WordReference
import qualified Logger as L
import Prelude as P
import Control.Concurrent.Thread.Delay
import Data.Int
import Control.Monad
import Data.Time.Clock
import Type
import Serializer
import Data.Text hiding (filter, map)
import Data.Maybe
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson
import GHC.Generics
import Data.Aeson
import Data.ByteString.Lazy hiding (elem, unpack, filter, map)
import Text.Pretty.Simple (pPrint, pString)
import GHC.Exts
import Debug.Trace
import Control.Exception
import Network.HTTP.Client (HttpException(HttpExceptionRequest))
import Data.Either
import Deserializer.Yandex
import Config.App
import Data.Monoid
import Control.Monad.IO.Class
import Deserializer.WordReference
import qualified Logger as L
import Prelude as P
import Control.Concurrent.Thread.Delay
import Data.Int
import Control.Monad
import Data.Time.Clock

select :: StaticConf -> Text -> IO (Maybe Value)
select sc word = do
  con <- connectPostgreSQL config
  responses <- query con q (Only word) :: IO ([Only Value])
  close con
  return $ resToMaybe responses
  where
    config :: B.ByteString
    config = B.append (B.append "dbname='" (Encoding.encodeUtf8 $ database sc)) "'"
    q = "SELECT response \
        \FROM wordreference w \
        \WHERE w.from = 'en' AND w.to = 'fr' AND w.word = ? \
        \LIMIT 1"
    resToMaybe :: [Only a] -> Maybe a
    resToMaybe responses =
      case responses of
        [Only i] -> Just i
        _ -> Nothing

available :: StaticConf -> IO [Text]
available sc = do
  con <- connectPostgreSQL config
  P.map unWrapOnly <$> query_ con q
  where
    config :: B.ByteString
    config = B.append (B.append "dbname='" (Encoding.encodeUtf8 $ database sc)) "'"
    q = "SELECT DISTINCT word \
        \FROM wordreference w \
        \WHERE w.from = 'en' AND w.to = 'fr'"
    unWrapOnly :: Only a -> a
    unWrapOnly (Only a) = a

insert :: [(Text, Maybe Value)] -> IO ()
insert keysAndValues = do
  con <- connectPostgreSQL config
  now <- getCurrentTime
  executeMany con q $ P.map (param now) keysAndValues
  return ()
  where
    config = "dbname='subtitre_dev'"
    param :: UTCTime -> (Text, Maybe Value) -> (Text, Text, Text, Maybe Value, UTCTime, UTCTime)
    param now (word, object) =
      ("en" :: Text, "fr" :: Text, word, object, now, now)
    q = "INSERT INTO wordreference (\"from\", \"to\", \"word\", \"response\", \"created_at\", \"updated_at\") \
        \ values (?, ?, ?, ?, ?, ?) "
