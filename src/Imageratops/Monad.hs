{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Imageratops.Monad where

import Imageratops.Prelude

import           Control.Monad.Base          (MonadBase(..), liftBaseDefault)
import           Control.Monad.Trans.Control (MonadBaseControl(..))
import qualified Network.AWS                 as AWS

import Imageratops.Env
import Imageratops.Storage as Storage

type Imageratops = ImageratopsT IO

newtype ImageratopsT m a
  = ImageratopsT
      { runImageratopsT :: ReaderT Env (ResourceT m) a }
  deriving
    ( Functor, Applicative, Monad
    , MonadIO
    , MonadReader Env
    , MonadThrow
    , MonadCatch
    )

runImageratops
  :: (MonadBaseControl IO m)
  => Env -> ImageratopsT m a
  -> m a
runImageratops env m =
  runResourceT
    $ runReaderT (runImageratopsT m) env

instance MonadTrans ImageratopsT where
  lift = ImageratopsT . lift . lift

instance (MonadBase IO m) => MonadBase IO (ImageratopsT m) where
  liftBase = liftBaseDefault

deriving instance (MonadResource m) => MonadResource (ImageratopsT m)

instance MonadBaseControl IO (ImageratopsT IO) where
  type StM (ImageratopsT IO) a = StM IO a
  liftBaseWith f =
    ImageratopsT $ ReaderT $ \env ->
      lift $ f $ \t -> runImageratops env t
  restoreM state =
    ImageratopsT $ ReaderT $ \_ -> lift $ pure state

instance AWS.MonadAWS (ImageratopsT IO) where
  liftAWS aws = do
    env <- asks envAwsEnv
    ImageratopsT $ AWS.runAWS env aws

instance Storage.MonadStorage Imageratops where
  read  = Storage.readFS  "/tmp/imageratops/"
  write = Storage.writeFS "/tmp/imageratops/"
