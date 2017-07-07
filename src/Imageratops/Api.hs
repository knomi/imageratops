{-# LANGUAGE OverloadedStrings #-}
module Imageratops.Api where

import           Imageratops.Prelude

import           Servant

import           Imageratops.Monad

type Api
  = Get '[JSON] Text

server :: ServerT Api Imageratops
server =
  pure "Hello"

