{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Imageratops.Image where

import Imageratops.Prelude

import qualified Control.Exception
import qualified Data.ByteString.Lazy       as ByteString.Lazy
import qualified Data.Text                  as Text
import qualified Servant
import           Servant.JuicyPixels        as Servant
  (BMP, GIF, JPEG, PNG, RADIANCE, TGA, TIFF)
import qualified Vision.Image               as Friday
import qualified Vision.Image.Storage.DevIL as Friday
import           Vision.Primitive           ((:.)(..), Z(..))
import qualified Vision.Primitive           as Friday

import Imageratops.Error    as Error
import Imageratops.Geometry as Geometry

newtype Image = Image
  { runImage :: Friday.StorageImage }

fromByteString
  :: (Friday.LoadImageType t, MonadThrow m)
  => t -> LByteString -> m Image
fromByteString format =
  either (throwM . Error.ImageDecodingFailed) (pure . Image)
    . Friday.loadBS format
    . ByteString.Lazy.toStrict

instance Servant.MimeUnrender Servant.OctetStream Image where
  mimeUnrender _ = fridayUnrender Friday.Autodetect

instance (KnownNat q, (q <=? 100) ~ 'True)
  => Servant.MimeUnrender (JPEG q) Image where
  mimeUnrender _ = fridayUnrender Friday.JPG

instance Servant.MimeUnrender PNG Image where
  mimeUnrender _ = fridayUnrender Friday.PNG

instance Servant.MimeUnrender BMP Image where
  mimeUnrender _ = fridayUnrender Friday.BMP

instance Servant.MimeUnrender GIF Image where
  mimeUnrender _ = fridayUnrender Friday.GIF

instance Servant.MimeUnrender RADIANCE Image where
  mimeUnrender _ = fridayUnrender Friday.HDR

instance Servant.MimeUnrender TGA Image where
  mimeUnrender _ = fridayUnrender Friday.TGA

instance Servant.MimeUnrender TIFF Image where
  mimeUnrender _ = fridayUnrender Friday.TIFF

fridayUnrender
  :: Friday.LoadImageType t
  => t -> LByteString -> Either String Image
fridayUnrender format =
  bimap show Image
    . Friday.loadBS format
    . ByteString.Lazy.toStrict

instance (KnownNat q, (q <=? 100) ~ 'True)
  => Servant.MimeRender (JPEG q) Image where
  mimeRender _ = fridayRender Friday.JPG

instance Servant.MimeRender PNG Image where
  mimeRender _ = fridayRender Friday.PNG

instance Servant.MimeRender BMP Image where
  mimeRender _ = fridayRender Friday.BMP

instance Servant.MimeRender RADIANCE Image where
  mimeRender _ = fridayRender Friday.HDR

instance Servant.MimeRender TGA Image where
  mimeRender _ = fridayRender Friday.TGA

instance Servant.MimeRender TIFF Image where
  mimeRender _ = fridayRender Friday.TIFF

fridayRender
  :: Friday.SaveBSImageType t
  => t -> Image -> LByteString
fridayRender format =
  either (Control.Exception.throw . Error.ImageEncodingFailed)
         ByteString.Lazy.fromStrict
    . Friday.saveBS format
    . runImage

scale :: FitTo -> Image -> Image
scale size (Friday.convert . runImage -> image :: Friday.RGBA) =
  Image
    $ Friday.convert . Friday.manifest
    $ Friday.resize Friday.Bilinear
        (Friday.ix2 (floor scaledHeight) (floor scaledWidth))
    $ (if cropping
       then Friday.delayed . Friday.crop rect
       else Friday.convert)
    $ image
  where
    cropping =
      case size of
        WidthHeight Cover _ _ -> True
        _ -> False

    (scaledWidth, scaledHeight) =
      case size of
        WidthHeight Cover w h -> (d w, d h)
        WidthHeight Contain w h ->
          let scalingFactor = min (d w / d imageWidth) (d h / d imageHeight) in
            (d imageWidth * scalingFactor, d imageHeight * scalingFactor)

        Width  w -> (d w, d w * srcRatio)
        Height h -> (d h / srcRatio, d h)

    Z :. imageHeight :. imageWidth =
      Friday.shape image

    srcRatio    = d imageHeight / d imageWidth
    targetRatio = scaledHeight / scaledWidth

    cropWidth
      | WidthHeight _ w h <- size =
          floor $ (d imageHeight / d h) * d w
      | otherwise = imageWidth

    cropHeight
      | WidthHeight _ w h <- size =
          floor $ (d imageWidth / d w) * d h
      | otherwise = imageHeight

    rect
      | targetRatio > srcRatio = Friday.Rect
          { rX = (imageWidth - cropWidth) `div` 2
          , rY = 0
          , rWidth  = cropWidth
          , rHeight = imageHeight
          }
      | otherwise = Friday.Rect
          { rX = 0
          , rY = (imageHeight - cropHeight) `div` 2
          , rWidth  = imageWidth
          , rHeight = cropHeight
          }

    d :: Int -> Double
    d = fromIntegral
