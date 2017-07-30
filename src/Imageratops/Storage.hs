module Imageratops.Storage where

import Imageratops.Prelude

import qualified Conduit
import qualified Data.ByteString.Lazy as LByteString
import qualified Network.AWS          as AWS
import qualified Network.AWS.S3       as S3
import           System.FilePath      ((</>))

import           Imageratops.ImageBody (ImageBody)
import qualified Imageratops.ImageBody as ImageBody
import           Imageratops.ImageId   (ImageId)
import qualified Imageratops.ImageId   as ImageId

class MonadStorage m where
  read  :: ImageId   -> m ImageBody
  write :: ImageBody -> m ImageId

readFS
  :: (MonadIO m, MonadThrow m)
  => FilePath -> ImageId -> m ImageBody
readFS dir imageId = do
  file <- liftIO $ LByteString.readFile filename
  ImageBody.decode file
  where
    filename = dir </> ImageId.toString imageId

writeFS :: (MonadIO m) => FilePath -> ImageBody -> m ImageId
writeFS dir imageBody = imageId <$ do
  liftIO $ LByteString.writeFile filename file
  where
    filename = dir </> ImageId.toString imageId
    file     = ImageBody.toByteString imageBody
    imageId  = ImageId.fromImage $ ImageBody.toImage imageBody

readS3 :: (MonadAWS m, MonadResource m) => S3.BucketName -> ImageId -> m ImageBody
readS3 bucketName imageId = do
  gors <- AWS.send $ S3.getObject bucketName $ imageKey imageId
  body <- AWS.sinkBody (gors ^. S3.gorsBody) Conduit.sinkLazy
  ImageBody.decode body

writeS3 :: (MonadAWS m, MonadResource m) => S3.BucketName -> ImageBody -> m ImageId
writeS3 bucketName imageBody = do
  _pors <- AWS.send $ S3.putObject bucketName (imageKey imageId) body
  pure imageId
  where
    body = AWS.toBody $ ImageBody.toByteString imageBody
    imageId = ImageId.fromImage $ ImageBody.toImage imageBody


imageKey :: ImageId -> S3.ObjectKey
imageKey = S3.ObjectKey . ImageId.toText
