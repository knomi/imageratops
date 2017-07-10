{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Imageratops.ImageId where

import Imageratops.Prelude

import qualified Codec.Picture        as Picture
import           Crypto.Hash.SHA1     as SHA1
import qualified Data.ByteString      as ByteString
import qualified Data.ByteString.Lazy as ByteString.Lazy
import           Data.UUID            (UUID)
import qualified Data.UUID            as UUID
import qualified Data.UUID.V5         as UUID.V5
import qualified Data.Vector.Storable as Vector

import Imageratops.Image

newtype ImageId = ImageId { runImageId :: UUID }
  deriving (Show, Eq, Ord, Read, NFData, Hashable)

toText :: ImageId -> Text
toText = UUID.toText . runImageId

toString :: ImageId -> String
toString = UUID.toString . runImageId


instance ToJSON ImageId where
  toJSON = toJSON . toText

instance FromJSON ImageId where
  parseJSON json = do
    text <- parseJSON json
    case UUID.fromText text of
      Nothing   -> fail "ImageId: expected a valid UUID"
      Just uuid -> pure $ ImageId uuid

fromImage :: Image -> ImageId
fromImage =
  ImageId
    -- 'generateNamed' performs poorly if feeded with a lot of data
    -- therefore we hash the input before feeding it there
    . UUID.V5.generateNamed UUID.V5.namespaceOID
    -- the following line was tricky to get right, when changing it
    -- ensure that nothing is prematurely forced and the words stream
    -- nicely
    . ByteString.unpack . SHA1.hashlazy . ByteString.Lazy.pack . Vector.toList
    . Picture.imageData . Picture.convertRGBA8
    . runImage
