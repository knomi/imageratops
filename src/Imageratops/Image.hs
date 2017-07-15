{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Imageratops.Image where

import Imageratops.Prelude

import qualified Control.Exception
import qualified Data.ByteString.Lazy       as ByteString.Lazy
import qualified Servant
import           Servant.JuicyPixels        as Servant
  (BMP, GIF, JPEG, PNG, RADIANCE, TGA, TIFF)
import qualified Vision.Image               as Friday
import qualified Vision.Image.Storage.DevIL as Friday
import           Vision.Primitive           ((:.)(..), Z(..))
import qualified Vision.Primitive           as Friday

import qualified Imageratops.Error as Error

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

data ScaleTo
  = Width  Int
  | Height Int
  | WidthHeight Int Int
  deriving (Show, Eq, Ord)

scale :: ScaleTo -> Image -> Image
scale size (Friday.convert . runImage -> image :: Friday.RGBA) =
  Image
    $ Friday.convert . Friday.manifest
    $ Friday.resize Friday.Bilinear
        (Friday.ix2 (floor scaledHeight) (floor scaledWidth))
    $ Friday.delayed . Friday.crop rect
    $ image
  where
    (scaledWidth, scaledHeight) =
      case size of
        Width       w   -> (toDouble w, toDouble w * srcRatio)
        Height        h -> (toDouble h / srcRatio, toDouble h)
        WidthHeight w h -> (toDouble w, toDouble h)

    Z :. imageHeight :. imageWidth =
      Friday.shape image

    srcRatio    = toDouble imageHeight / toDouble imageWidth
    targetRatio = scaledHeight / scaledWidth

    cropWidth
      | WidthHeight w h <- size =
          floor $ (toDouble imageHeight / toDouble h) * toDouble w
      | otherwise = imageWidth

    cropHeight
      | WidthHeight w h <- size =
          floor $ (toDouble imageWidth / toDouble w) * toDouble h
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

    toDouble :: Int -> Double
    toDouble = fromIntegral
