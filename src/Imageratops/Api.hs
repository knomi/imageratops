{-# LANGUAGE OverloadedStrings #-}
module Imageratops.Api where

import Imageratops.Prelude

import Servant
import Servant.OptionalReqBody

import           Imageratops.Api.QueryParams as QueryParams
import qualified Imageratops.Fetch           as Fetch
import           Imageratops.Geometry        as Geometry
import qualified Imageratops.Image           as Image
import           Imageratops.ImageBody       (ImageBody)
import qualified Imageratops.ImageBody       as ImageBody
import           Imageratops.ImageId         (ImageId)
import           Imageratops.Monad
import           Imageratops.Storage         as Storage

type OutputTypes = [ Image.Origin, Image.JPEG, Image.PNG ]
type InputTypes  = [ Image.JPEG, Image.PNG ]

type Api =
  "_status" :> Get '[JSON] Text
  :<|>
  Capture "image-id" ImageId
    :> QueryParams.FitTo
    :> Get OutputTypes ImageBody
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

    getImage :: ImageId -> Maybe Geometry.FitTo -> Imageratops ImageBody
    getImage imageId fitTo = do
      imageBody <- Storage.read imageId
      let image = ImageBody.toImage imageBody
      case fitTo of
        Nothing -> pure imageBody
        Just fitTo_ -> do
          either throwM pure
            $ ImageBody.fromImage (ImageBody.toFormat imageBody)
            $ Image.scale fitTo_ image

    addImage :: Maybe ImageBody -> Maybe Fetch.Url -> Imageratops ImageId
    addImage (Just imageBody) _ = do
      Storage.write imageBody
    addImage _ (Just url) = do
      imageBody <- Fetch.fromUrl url
      addImage (Just imageBody) (Just url)

