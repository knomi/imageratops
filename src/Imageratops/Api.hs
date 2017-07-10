{-# LANGUAGE OverloadedStrings #-}
module Imageratops.Api where

import Imageratops.Prelude

import           Data.Text                (Text)
import           Servant
import           Servant.JuicyPixels      (BMP, GIF, JPEG, PNG)
import           Servant.OptionalReqBody  (OptionalReqBody)
import qualified Vision.Image             as Friday
import qualified Vision.Image.JuicyPixels as Friday
import qualified Vision.Primitive         as Friday

import           Imageratops.Image     (Image(..))
import qualified Imageratops.Image     as Image
import           Imageratops.ImageBody (ImageBody(..))
import qualified Imageratops.ImageBody as ImageBody
import           Imageratops.ImageId   (ImageId)
import qualified Imageratops.ImageId   as ImageId
import           Imageratops.Monad

type InputTypes  = [JPEG 100, PNG, BMP, GIF, OctetStream]
type OutputTypes = [JPEG 100, PNG, BMP, GIF]

type Api =
  "id"
    :> ReqBody InputTypes ImageBody
    :> Post '[JSON] ImageId
    :<|>
  "convert"
    :> ReqBody InputTypes ImageBody
    :> Post OutputTypes Image
    :<|>
  "scale"
    :> ReqBody InputTypes ImageBody
    :> QueryParam "width"  Int
    :> QueryParam "height" Int
    :> Post OutputTypes Image

server :: ServerT Api Imageratops
server =
  identify
    :<|>
  convert
    :<|>
  scale
  where
    identify imageBody =
      pure $ ImageId.fromImage $ ImageBody.toImage imageBody

    convert imageBody =
      pure $ ImageBody.toImage imageBody

    scale imageBody mw mh =
      imageBody
        & ImageBody.toImage
        & case (mw, mh) of
            (Just w, Just h)  -> Image.scale $ Image.WidthHeight w h
            (Just w, Nothing) -> Image.scale $ Image.Width w
            (Nothing, Just h) -> Image.scale $ Image.Height h
            _ -> id
        & pure
