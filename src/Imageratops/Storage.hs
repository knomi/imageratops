module Imageratops.Storage where

import Imageratops.Prelude

import qualified Data.ByteString.Lazy as LByteString
import qualified Data.Text            as Text
import qualified Network.AWS.S3       as S3
import           System.FilePath      ((</>))

import qualified Imageratops.Error     as Error
import           Imageratops.ImageBody (ImageBody)
import qualified Imageratops.ImageBody as ImageBody
import           Imageratops.ImageId   (ImageId)
import qualified Imageratops.ImageId   as ImageId

class MonadStorage m where
  read  :: ImageId   -> m ImageBody
  write :: ImageBody -> m ImageId

data S3Config = S3Config
  { s3ConfigBucketName :: S3.BucketName }
  deriving (Show, Eq, Ord)

readFS
  :: (MonadIO m, MonadThrow m)
  => FilePath -> ImageId -> m ImageBody
readFS dir imageId = do
  file <- liftIO $ LByteString.readFile filename
  case ImageBody.imageBody file of
    Right imageBody ->
      pure imageBody
    Left err ->
      throwM $ Error.ImageDecodingFailed $ Text.pack err
  where
    filename = dir </> ImageId.toString imageId

writeFS :: (MonadIO m) => FilePath -> ImageBody -> m ImageId
writeFS dir imageBody = imageId <$ do
  liftIO $ LByteString.writeFile filename file
  where
    filename = dir </> ImageId.toString imageId
    file     = ImageBody.toByteString imageBody
    imageId  = ImageId.fromImage $ ImageBody.toImage imageBody

imageKey :: ImageId -> S3.ObjectKey
imageKey = S3.ObjectKey . ImageId.toText
