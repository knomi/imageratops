{-# LANGUAGE OverloadedStrings #-}
module Imageratops.Fetch where

import Imageratops.Prelude

import qualified Data.Text           as Text
import           Network.HTTP.Client as Http
import           Network.URI         (URI)
import qualified Network.URI         as URI

import Imageratops.ImageBody as ImageBody

newtype Url = Url { runUrl :: URI }
  deriving (Show, Eq, Ord)

instance FromHttpApiData Url where
  parseQueryParam =
    maybe (Left "Url: not a valid URI") (Right . Url)
      . URI.parseURI . Text.unpack

fromUrl
  :: ( MonadIO m
     , HasHttpManager r
     , MonadReader r m
     , MonadThrow m
     ) => Url -> m ImageBody
fromUrl (Url url) =
  fromHttpRequest =<< Http.parseUrlThrow (URI.uriToString id url "")

fromHttpRequest
  :: ( MonadIO m
     , HasHttpManager r
     , MonadReader r m
     , MonadThrow m
     ) => Http.Request -> m ImageBody
fromHttpRequest request = do
  manager <- asks getHttpManager
  response <- liftIO $ Http.httpLbs request manager
  let body = Http.responseBody response
  ImageBody.fromByteString body

--urls <- Text.lines <$> Text.readFile "/home/zudov/images.txt"
--let postConcurrently = Async.mapConcurrently $ \url -> do { req <- Http.parseRequest "http://localhost:8080"; Http.httpLbs (Http.setQueryString [("url", Just $ Text.encodeUtf8 url)] $ req { Http.method = "POST" }) manager; }
--let post urls' = if null urls' then pure () else (mapM_ print =<< postConcurrently (take 8 urls')) *> post (drop 8 urls')
