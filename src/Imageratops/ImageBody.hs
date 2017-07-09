{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Imageratops.ImageBody where

import Imageratops.Prelude

import qualified Codec.Picture        as Picture
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified Servant
import           Servant.JuicyPixels  (BMP, GIF, JPEG, PNG, RADIANCE, TGA, TIFF)

newtype ImageBody = ImageBody
  { runImageBody :: Picture.DynamicImage }
  deriving
    ( Servant.MimeRender BMP
    , Servant.MimeRender GIF
    , Servant.MimeRender PNG
    , Servant.MimeRender RADIANCE
    , Servant.MimeRender TIFF
    , Servant.MimeRender TGA
    , Servant.MimeUnrender BMP
    , Servant.MimeUnrender GIF
    , Servant.MimeUnrender PNG
    , Servant.MimeUnrender RADIANCE
    , Servant.MimeUnrender TIFF
    , Servant.MimeUnrender TGA
    )

deriving instance
  (KnownNat n, (n <=? 100) ~ 'True)
  => Servant.MimeUnrender (JPEG n) ImageBody

deriving instance
  (KnownNat n, (n <=? 100) ~ 'True)
  => Servant.MimeRender (JPEG n) ImageBody

instance Servant.MimeUnrender Servant.OctetStream ImageBody where
  mimeUnrender _ =
    fmap ImageBody
      . Picture.decodeImage
      . ByteString.Lazy.toStrict
