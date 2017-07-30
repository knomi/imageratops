{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Imageratops.ImageBody
  ( ImageBody
  , decode
  , fromByteString
  , fromImage
  , toFormat
  , toImage
  , toByteString
  ) where

import Imageratops.Prelude

import qualified Control.Exception
import qualified Servant
import qualified Servant.API.ContentTypes as Servant

import qualified Imageratops.Error as Error
import           Imageratops.Image (Image)
import qualified Imageratops.Image as Image

data ImageBody = ImageBody
  { image      :: Image
  , format     :: Image.Format
  , bytestring :: LByteString
  }

decode :: (MonadThrow m) => LByteString -> m ImageBody
decode bs = either throwM pure decoder
  where
    decoder :: Either Error.ImageDecodingFailed ImageBody
    decoder =
      catchError (fromByteString Image.JPEG bs)
        (\_ -> fromByteString Image.PNG bs)

toImage :: ImageBody -> Image
toImage = image

toFormat :: ImageBody -> Image.Format
toFormat = format

toByteString :: ImageBody -> LByteString
toByteString = bytestring

fromByteString
  :: (MonadError Error.ImageDecodingFailed m)
  => Image.Format -> LByteString -> m ImageBody
fromByteString format bytestring = do
  image <- Image.fromByteString format bytestring
  pure ImageBody{..}

fromImage
  :: (MonadError Error.ImageEncodingFailed m)
  => Image.Format -> Image -> m ImageBody
fromImage format image = do
  bytestring <- Image.toByteString format image
  pure ImageBody{..}


instance {-# OVERLAPPING #-} (Servant.AllCTRender cts ImageBody)
  => Servant.AllCTRender
       (Image.Content 'Identity ': cts)
       ImageBody
  where
  handleAcceptH _ (Servant.AcceptHeader "*/*") imageBody =
     pure ( case format imageBody of
                   Image.PNG  -> "image/png"
                   Image.JPEG -> "image/jpeg"
          , bytestring imageBody
          )
  handleAcceptH _ accept imageBody =
    Servant.handleAcceptH (Proxy :: Proxy cts) accept imageBody


instance Servant.MimeUnrender (Image.Content 'Image.JPEG) ImageBody where
  mimeUnrender _ct = first show . fromByteString Image.JPEG

instance Servant.MimeUnrender (Image.Content 'Image.PNG) ImageBody where
  mimeUnrender _ct = first show . fromByteString Image.PNG

instance Servant.MimeRender (Image.Content 'Image.JPEG) ImageBody where
  mimeRender _ct ImageBody{format = Image.JPEG,..} = bytestring
  mimeRender _ct imageBody =
    either Control.Exception.throw bytestring
      $ fromImage Image.JPEG
      $ image imageBody

instance Servant.MimeRender (Image.Content 'Image.PNG) ImageBody where
  mimeRender _ct ImageBody{format = Image.PNG,..} = bytestring
  mimeRender _ct imageBody =
    either Control.Exception.throw bytestring
      $ fromImage Image.PNG
      $ image imageBody

instance Servant.MimeRender (Image.Content 'Identity) ImageBody where
  mimeRender _ct = bytestring
