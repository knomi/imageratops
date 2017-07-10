{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Imageratops.ImageBody
  ( ImageBody
  , imageBody
  , toImage
  , toByteString
  ) where

import Imageratops.Prelude

import qualified Codec.Picture        as JP
import qualified Data.ByteString.Lazy as LByteString
import qualified Servant

import Imageratops.Image

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

imageBody :: (MonadError String m) => LByteString -> m ImageBody
imageBody bs = do
  image <- either throwError (pure . Image)
             $ JP.decodeImage $ LByteString.toStrict bs
  pure $ ImageBody image bs

instance
  ( Servant.Accept contentType
  , Servant.MimeUnrender contentType Image
  ) => Servant.MimeUnrender contentType ImageBody
  where
  mimeUnrender contentType bytestring = do
    image <- Servant.mimeUnrender contentType bytestring
    pure $ ImageBody {..}
