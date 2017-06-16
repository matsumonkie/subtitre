{-# LANGUAGE OverloadedStrings #-}

module TestUtil (
  fakeHttpResponse
) where

import Network.Wreq
import qualified Network.HTTP.Client.Internal as HTTP
import Network.HTTP.Types.Status
import Network.HTTP.Types.Version
import Data.ByteString.Lazy hiding (elem)

fakeHttpResponse :: ByteString -> Response ByteString
fakeHttpResponse body =
  HTTP.Response { HTTP.responseStatus    = status200
                , HTTP.responseVersion   = http11
                , HTTP.responseHeaders   = []
                , HTTP.responseBody      = body
                , HTTP.responseCookieJar = HTTP.createCookieJar []
                , HTTP.responseClose'    = HTTP.ResponseClose { HTTP.runResponseClose = return () }
                }
