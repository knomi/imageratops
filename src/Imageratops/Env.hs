module Imageratops.Env where

import           Imageratops.Prelude

import qualified Network.AWS         as AWS

import           Imageratops.Config  (Config)

data Env = Env
  { envConfig :: Config
  , envAwsEnv :: AWS.Env
  }

instance AWS.HasEnv Env where
  environment = lens envAwsEnv (\env awsEnv -> env { envAwsEnv = awsEnv})
