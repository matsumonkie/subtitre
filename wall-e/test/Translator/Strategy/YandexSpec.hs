{-# LANGUAGE OverloadedStrings #-}

module Translator.Strategy.YandexSpec (main, spec) where

import Common
import Prelude

import Type
import Data.Monoid
import qualified Data.Text as T
import Test.Hspec
import qualified Data.ByteString.Lazy as BSL
import Network.Wreq
import qualified Network.HTTP.Client.Internal as HTTP

import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Translator.Strategy.Yandex
--import TestUtil

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Translator/Strategy/Yandex" $ do
    describe "reads and parse responses correctly" $ do
      it "translate correctly" $ do
        let wi = ("stirrings", "stirring", Noun, Normal)
        Translations'(_, translations) <- translate (fetch "yandex.stirring.json") wi
        translations `shouldBe` ["agitation"]

fetch :: String -> T.Text -> IO (Maybe (Response BSL.ByteString))
fetch file _ = do
  body' <- body
  return $ Just (fakeHttpResponse body')
  where
    body :: IO BSL.ByteString
    body = BSL.readFile path
    path :: FilePath
    path = "test/response/" <> file

fakeHttpResponse :: BSL.ByteString -> Response BSL.ByteString
fakeHttpResponse body =
  HTTP.Response { HTTP.responseStatus    = status200
                , HTTP.responseVersion   = http11
                , HTTP.responseHeaders   = []
                , HTTP.responseBody      = body
                , HTTP.responseCookieJar = HTTP.createCookieJar []
                , HTTP.responseClose'    = HTTP.ResponseClose { HTTP.runResponseClose = return () }
                }
