{-# LANGUAGE OverloadedStrings #-}
module Imageratops.Api where

import Imageratops.Prelude

import Servant
import Servant.JuicyPixels     (BMP, GIF, JPEG, PNG)
import Servant.OptionalReqBody

import qualified Imageratops.Fetch     as Fetch
import           Imageratops.Image     (Image(..))
import qualified Imageratops.Image     as Image
import           Imageratops.ImageBody (ImageBody)
import qualified Imageratops.ImageBody as ImageBody
import           Imageratops.ImageId   (ImageId)
import           Imageratops.Monad
import           Imageratops.Storage   as Storage

type InputTypes  = [JPEG 100, PNG, BMP, GIF, OctetStream]
type OutputTypes = [JPEG 100, PNG, BMP]

type Api =
  Capture "image-id" ImageId
    :> QueryParam "width"  Int
    :> QueryParam "height" Int
    :> Get OutputTypes Image
  :<|>
  OptionalReqBody InputTypes ImageBody
    :> QueryParam "url" Fetch.Url
    :> Post '[JSON] ImageId

server :: ServerT Api Imageratops
server =
  getImage
    :<|>
  addImage
  where
    getImage :: ImageId -> Maybe Int -> Maybe Int -> Imageratops Image
    getImage imageId width height = do
      imageBody <- Storage.read imageId
      let image = ImageBody.toImage imageBody
      pure $ maybe image (`Image.scale` image) size
      where
        size =
         (Image.WidthHeight <$> width <*> height)
           <|>
         (Image.Width <$> width)
           <|>
         (Image.Height <$> height)

    addImage :: Maybe ImageBody -> Maybe Fetch.Url -> Imageratops ImageId
    addImage (Just imageBody) _ = do
      Storage.write imageBody
    addImage _ (Just url) = do
      imageBody <- Fetch.fromUrl url
      addImage (Just imageBody) (Just url)

