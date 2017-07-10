module Imageratops.Error where

import Imageratops.Prelude

data ImageDecodingFailed = ImageDecodingFailed Text
  deriving (Show, Eq, Ord)

instance Exception ImageDecodingFailed
