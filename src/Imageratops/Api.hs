{-# LANGUAGE OverloadedStrings #-}
module Imageratops.Api where

import Imageratops.Prelude

import Servant
import Servant.JuicyPixels     (BMP, GIF, JPEG, PNG)
import Servant.OptionalReqBody

import           Imageratops.Api.QueryParams as QueryParams
import qualified Imageratops.Fetch           as Fetch
import           Imageratops.Geometry        as Geometry
import           Imageratops.Image           (Image(..))
import qualified Imageratops.Image           as Image
import           Imageratops.ImageBody       (ImageBody)
import qualified Imageratops.ImageBody       as ImageBody
import           Imageratops.ImageId         (ImageId)
import           Imageratops.Monad
import           Imageratops.Storage         as Storage

type InputTypes  = [JPEG 100, PNG, BMP, GIF, OctetStream]
type OutputTypes = [JPEG 100, PNG, BMP]

type Api =
  "_status" :> Get '[JSON] Text
  :<|>
  Capture "image-id" ImageId
    :> QueryParams.FitTo
    :> Get OutputTypes Image
  :<|>
  OptionalReqBody InputTypes ImageBody
    :> QueryParam "url" Fetch.Url
    :> Post '[JSON] ImageId

server :: ServerT Api Imageratops
server =
  getStatus
    :<|>
  getImage
    :<|>
  addImage
  where
    getStatus :: Imageratops Text
    getStatus = pure "We are fine"

    getImage :: ImageId -> Maybe Geometry.FitTo -> Imageratops Image
    getImage imageId fitTo = do
      imageBody <- Storage.read imageId
      let image = ImageBody.toImage imageBody
      pure $ maybe id Image.scale fitTo image

    addImage :: Maybe ImageBody -> Maybe Fetch.Url -> Imageratops ImageId
    addImage (Just imageBody) _ = do
      Storage.write imageBody
    addImage _ (Just url) = do
      imageBody <- Fetch.fromUrl url
      addImage (Just imageBody) (Just url)

