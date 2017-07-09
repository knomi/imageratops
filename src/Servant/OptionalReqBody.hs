{-# LANGUAGE OverloadedStrings #-}
module Servant.OptionalReqBody where

import Imageratops.Prelude

import           Codec.Picture.Types                        as Picture
import qualified Data.ByteString.Lazy                       as LByteString
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import qualified Data.Text.Encoding                         as Text
import qualified Network.HTTP.Types.Header                  as Header
import qualified Network.Wai                                as Wai
import           Servant
import           Servant.API.ContentTypes
  (AllCTUnrender, canHandleCTypeH)
import           Servant.JuicyPixels
  (BMP, GIF, JPEG, PNG, RADIANCE, TGA, TIFF)
import           Servant.Server.Internal.RoutingApplication as Routing

data OptionalReqBody cts a

instance
  ( AllCTUnrender cts a
  , HasServer api context
  , reqBodyApi ~ (ReqBody cts a :> api)
  )
  => HasServer (OptionalReqBody cts a :> api) context where
  type ServerT (OptionalReqBody cts a :> api) m =
    Maybe a -> ServerT api m
  route _ context subserver =
    route @api Proxy context
      $ addBodyCheck subserver
          (withRequest contentTypeCheck)
          bodyCheck
    where
      contentTypeCheck
        :: Wai.Request -> DelayedIO (Maybe (LByteString -> Either String a))
      contentTypeCheck request
        | hasMessageBody && not isEmptyMessageBody =
            Just <$> getContentHandler
        | otherwise =
            pure Nothing
        where
          headers = Wai.requestHeaders request
          hContentType = headers & lookup Header.hContentType
          hContentLength = headers & lookup Header.hContentLength
          hTransferEncoding = headers & lookup Header.hTransferEncoding

          -- https://www.w3.org/Protocols/rfc2616/rfc2616-sec4.html
          -- 4.3 Message body
          -- > The presence of a message-body in a request is
          -- > signaled by the inclusion of a Content-Length
          -- > or Transfer-Encoding header field in the request's
          -- > message-headers.
          hasMessageBody =
            isJust (hContentLength <|> hTransferEncoding)

          isEmptyMessageBody = fromMaybe False $ do
            contentLength <- parseContentLength =<< hContentLength
            pure $ contentLength <= 0
            where
              parseContentLength =
                (readMaybe :: String -> Maybe Double)
                  . Text.unpack . Text.decodeUtf8

          -- when Content-Type is not provided we assume it's octet-stream
          orOctetStream = fromMaybe "application/octet-stream"

          getContentHandler
            :: DelayedIO (LByteString -> Either String a)
          getContentHandler =
             maybe (delayedFailFatal err415) pure
               $ canHandleCTypeH (Proxy :: Proxy cts)
                   $ LByteString.fromStrict
                   $ orOctetStream hContentType

      bodyCheck
        :: Maybe (LByteString -> Either String a)
        -> DelayedIO (Maybe a)
      bodyCheck = traverse $ \contentHandler -> withRequest $ \request -> do
        body <- liftIO (Wai.lazyRequestBody request)
        case contentHandler body of
          Left e -> delayedFailFatal err400
            { errBody = LByteString.fromStrict
                      $ Text.encodeUtf8 $ Text.pack e
            }
          Right a -> pure a
