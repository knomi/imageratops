{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Imageratops.Image where

import Imageratops.Prelude

import qualified Codec.Picture            as JP
import qualified Data.ByteString.Lazy     as ByteString.Lazy
import qualified Data.Text                as Text
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

-- | Specifies how the image should match the box.
data Fit
  = Contain
  | Cover
  deriving (Show, Eq, Ord)

instance FromHttpApiData Fit where
  parseQueryParam (Text.toLower -> "cover")   = pure Cover
  parseQueryParam (Text.toLower -> "contain") = pure Contain
  parseQueryParam _ = throwError "FitBy: expected 'cover' or 'contain'"

data ScaleTo
  = Width  Int
  | Height Int
  | WidthHeight Fit Int Int
  deriving (Show, Eq, Ord)

scale :: ScaleTo -> Image -> Image
scale size (JP.convertRGBA8 . runImage -> image@JP.Image{..}) =
  image
    & Friday.toFridayRGBA
    & (if   cropping
       then Friday.delayed . Friday.crop rect
       else Friday.convert)
    & Friday.resize Friday.Bilinear
        (Friday.ix2 (floor scaledHeight) (floor scaledWidth))
    & Friday.manifest
    & Friday.toJuicyRGBA
    & JP.ImageRGBA8
    & Image
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
