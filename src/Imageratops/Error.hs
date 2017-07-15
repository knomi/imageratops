module Imageratops.Error where

import qualified Vision.Image.Storage.DevIL as Friday

import Imageratops.Prelude

data ImageDecodingFailed = ImageDecodingFailed Friday.StorageError
  deriving (Show, Eq)

instance Exception ImageDecodingFailed

data ImageEncodingFailed = ImageEncodingFailed Friday.StorageError
  deriving (Show, Eq)

instance Exception ImageEncodingFailed
