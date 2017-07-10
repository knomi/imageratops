{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Imageratops.Image where

import Imageratops.Prelude

import qualified Codec.Picture            as JP
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import qualified Servant
import           Servant.JuicyPixels
  (BMP, GIF, JPEG, PNG, RADIANCE, TGA, TIFF)
import qualified Vision.Image             as Friday
import qualified Vision.Image.JuicyPixels as Friday
import qualified Vision.Primitive         as Friday

newtype Image = Image
  { runImage :: JP.DynamicImage }
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

instance Servant.MimeUnrender Servant.OctetStream Image where
  mimeUnrender _ =
    fmap Image
      . JP.decodeImage
      . ByteString.Lazy.toStrict

deriving instance (KnownNat q, (q <=? 100) ~ 'True)
  => Servant.MimeUnrender (JPEG q) Image

deriving instance (KnownNat q, (q <=? 100) ~ 'True)
  => Servant.MimeRender (JPEG q) Image

data ScaleTo
  = Width  Int
  | Height Int
  | WidthHeight Int Int
  deriving (Show, Eq, Ord)

scale :: ScaleTo -> Image -> Image
scale size (JP.convertRGBA8 . runImage -> image@JP.Image{..}) =
  image
    & Friday.toFridayRGBA
    & Friday.crop rect
    & Friday.delayed
    & Friday.resize Friday.Bilinear
        (Friday.ix2 (floor scaledHeight) (floor scaledWidth))
    & Friday.manifest
    & Friday.toJuicyRGBA
    & JP.ImageRGBA8
    & Image
  where
    (scaledWidth, scaledHeight) =
      case size of
        Width       w   -> (toDouble w, toDouble w * srcRatio)
        Height        h -> (toDouble h / srcRatio, toDouble h)
        WidthHeight w h -> (toDouble w, toDouble h)

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
