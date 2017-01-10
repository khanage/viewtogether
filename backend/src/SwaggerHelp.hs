{-# language FlexibleInstances #-}
module SwaggerHelp
  ( SwaggerAPI
  , swaggerServer
  ) where

import Control.Lens
import Imports hiding (Handler)
import Servant.Swagger
import Data.Swagger
import Servant.API.BasicAuth (BasicAuthData(BasicAuthData))
import Servant
import Claims
 
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
  :<|> "swagger" :> Raw
  :<|> "images" :> Raw
  :<|> "css" :> Raw
  :<|> "lib" :> Raw

swaggerServer
  :: HasSwagger a
  => Proxy a
  -> Server SwaggerAPI
swaggerServer api = getSwaggerData api
  :<|> serveDirectory "./static/swagger-ui/dist"
  :<|> serveDirectory "./static/images"
  :<|> serveDirectory "./static/css"
  :<|> serveDirectory "./static/lib"

getSwaggerData
  :: HasSwagger a
  => Proxy a
  -> Handler Swagger
getSwaggerData api = pure
  $ toSwagger api
  & info.title .~ "View together api"
  & info.description .~ Just "Backing services for all the viewtogether things."

instance (HasSwagger sub, ToParamSchema a)
  => HasSwagger (BasicAuth realm a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam basicAuthHeader
    & security .~ requirements
    where
      requirements = []
      basicAuthHeader = mempty
        & name .~ "Authorization"
        & required .~ Just True
        & description .~ Just "Basic base64(name:password)"
        & schema .~ ParamOther (mempty
             & in_ .~ ParamHeader
             & paramSchema .~ toParamSchema (Proxy :: Proxy a))

addParam :: Param -> Swagger -> Swagger
addParam param = allOperations.parameters %~ (Inline param :)

instance (HasSwagger sub)
  => HasSwagger (AuthProtect "claims" :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam basicAuthHeader
    where
      basicAuthHeader = mempty
        & name .~ "claims"
        & required .~ Just True
        & description .~ Just "A jwt in either a claims query param or header"
        & schema .~ ParamOther (mempty
             & in_ .~ ParamHeader
             & paramSchema .~ toParamSchema (Proxy :: Proxy Text))
