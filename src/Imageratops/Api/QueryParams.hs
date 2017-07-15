module Imageratops.Api.QueryParams where

import Imageratops.Prelude

import Servant as Servant

import qualified Imageratops.Geometry as Geometry

data FitTo

instance
  ( HasServer api ctx
  , fitApi ~ ( QueryParam "width"  Int
               :> QueryParam "height" Int
               :> QueryParam "fit"    Geometry.Fit
               :> api
             )
  ) => HasServer (FitTo :> api) ctx
  where
    type ServerT (FitTo :> api) m =
      Maybe Geometry.FitTo -> ServerT api m
    route _ ctx =
      route @fitApi Proxy ctx . fmap mapServer
      where
        mapServer
          :: Server (FitTo :> api)
          -> Server fitApi
        mapServer fitServer =
          \width height fit -> fitServer $ asum
             [ Geometry.WidthHeight (fromMaybe Geometry.Cover fit)
                 <$> width
                 <*> height
             , Geometry.Width  <$> width
             , Geometry.Height <$> height
             ]
