{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Imageratops.ImageBody
  ( ImageBody
  , fromByteString
  , toImage
  , toByteString
  ) where

import Imageratops.Prelude

import qualified Servant
import qualified Vision.Image.Storage.DevIL as Friday

import           Imageratops.Image (Image)
import qualified Imageratops.Image as Image

-- | An image along with the original bytestring from which that image
--   was decoded.
data ImageBody = ImageBody
  { image      :: Image
  , bytestring :: LByteString
  }

toImage :: ImageBody -> Image
toImage = image

toByteString :: ImageBody -> LByteString
toByteString = bytestring

fromByteString :: (MonadThrow m) => LByteString -> m ImageBody
fromByteString bs = do
  image <- Image.fromByteString Friday.Autodetect bs
  pure $ ImageBody image bs

instance
  ( Servant.Accept contentType
  , Servant.MimeUnrender contentType Image
  ) => Servant.MimeUnrender contentType ImageBody
  where
  mimeUnrender contentType bytestring = do
    image <- Servant.mimeUnrender contentType bytestring
    pure $ ImageBody{..}
