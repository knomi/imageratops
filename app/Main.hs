{-# LANGUAGE OverloadedStrings #-}
module Main where

import Imageratops.Prelude

import qualified Network.AWS                          as AWS
import qualified Network.Wai                          as Wai
import qualified Network.Wai.Handler.Warp             as Warp
import qualified Network.Wai.Middleware.RequestLogger as Wai
import           Servant                              ((:~>))
import qualified Servant                              as Servant
import           System.IO                            (stderr)

import Imageratops.Api    (Api, server)
import Imageratops.Config
import Imageratops.Env
import Imageratops.Monad

main :: IO ()
main = do
  logger <- AWS.newLogger AWS.Debug stderr
  awsEnv <- AWS.newEnv AWS.Discover
  Warp.run 8080
    $ Wai.logStdoutDev
    $ app
    $ Env { envAwsEnv = awsEnv, envConfig = Config }

app :: Env -> Wai.Application
app env =
  Servant.serve @Api Proxy
    $ Servant.enter enterNat server
  where
    enterNat :: Imageratops :~> Servant.Handler
    enterNat =
      Servant.NT
        $ Servant.Handler
        . ExceptT . try
        . runImageratops env
