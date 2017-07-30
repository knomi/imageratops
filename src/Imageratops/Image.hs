{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Imageratops.Image where

import Imageratops.Prelude

import qualified Data.ByteString.Lazy         as ByteString.Lazy
import           Data.Ratio                   (Ratio, (%))
import           Network.HTTP.Media.MediaType ((//))
import qualified Servant
import qualified Vision.Image                 as Friday
import qualified Vision.Image.Storage.DevIL   as Friday
import           Vision.Primitive             ((:.)(..), Z(..))
import qualified Vision.Primitive             as Friday

import Imageratops.Error    as Error
import Imageratops.Geometry as Geometry

data Content format

type JPEG = Content 'JPEG
type PNG  = Content 'PNG
type Origin = Content 'Identity

data Format
  = JPEG
  | PNG
  deriving (Show, Eq, Ord, Enum, Bounded)

instance Servant.Accept (Content 'Identity) where
  contentType _ = "*" // "*"

instance Servant.Accept (Content 'JPEG) where
  contentType _ = "image" // "jpeg"

instance Servant.Accept (Content 'PNG) where
  contentType _ = "image" // "png"

newtype Image = Image
  { runImage :: Friday.StorageImage }

fromByteString
  :: (MonadError Error.ImageDecodingFailed m)
  => Format -> LByteString -> m Image
fromByteString format =
  either (throwError . Error.ImageDecodingFailed) (pure . Image)
    . reader
    . ByteString.Lazy.toStrict
  where
    reader = case format of
      JPEG -> Friday.loadBS Friday.JPG
      PNG  -> Friday.loadBS Friday.PNG

toByteString
  :: (MonadError Error.ImageEncodingFailed m)
  => Format -> Image -> m LByteString
toByteString format =
  either (throwError . Error.ImageEncodingFailed)
         (pure . ByteString.Lazy.fromStrict)
    . writer
    . runImage
  where
    writer = case format of
      JPEG -> Friday.saveBS Friday.JPG
      PNG  -> Friday.saveBS Friday.PNG

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
          let scalingFactor = min (w % imageWidth) (h % imageHeight) in
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

    d :: Int -> Ratio Int
    d a = a % 1
