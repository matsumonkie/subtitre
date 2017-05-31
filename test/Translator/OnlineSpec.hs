{-# LANGUAGE OverloadedStrings #-}

module Translator.OnlineSpec (main, spec) where

import Type
import Data.Monoid
import Data.Text hiding (map)
import Prelude hiding (readFile)
import Test.Hspec
import Translator.Online
import Data.ByteString.Lazy hiding (elem)
import Network.Wreq
import qualified Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
--import TestUtil

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Translator/Online" $ do
    describe "parse" $ do
      it "end with newline" $ do
        let wi = ("stirrings", "stirring", Noun, Normal)
        Translations(_, translations) <- translate wi $ fetch "yandex.stirring.json"
        translations `shouldBe` ["agitation"]

fetch :: String -> Text -> IO (Maybe (Response ByteString))
fetch file _ = do
  body' <- body
  return $ Just (fakeHttpResponse body')
  where
    body :: IO ByteString
    body = readFile path
    path :: FilePath
    path = "test/response/" <> file

fakeHttpResponse :: ByteString -> Response ByteString
fakeHttpResponse body =
  HTTP.Response { HTTP.responseStatus    = status200
                , HTTP.responseVersion   = http11
                , HTTP.responseHeaders   = []
                , HTTP.responseBody      = body
                , HTTP.responseCookieJar = HTTP.createCookieJar []
                , HTTP.responseClose'    = HTTP.ResponseClose { HTTP.runResponseClose = return () }
                }
