{-# LANGUAGE DataKinds, TemplateHaskell, TypeOperators #-}
module Lib
    ( startApp
    ) where

import Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy     as BSL
import           Data.Swagger
import           Handlers
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Swagger          (toSwagger)

import           Imports                  hiding (Handler)

type AppServer a = ServerT a App
type API = Get '[JSON] Text
     :<|> "users" :> Get '[JSON] [User]
     :<|> "users" :> ReqBody '[JSON] UserEdit
                  :> Post '[JSON] ()

server :: AppServer API
server = rootHandler
    :<|> getUsersHandler
    :<|> postUserHandler

startApp
  :: (MonadLoggerIO m)
  => Port
  -> AppConfig
  -> m ()
startApp port config = do
  $logInfo $ "Running on port " <> tshow port <> "."
  logger <- askLoggerIO
  liftIO $ run port $ app logger config

appToHandler
  :: AppConfig
  -> Logger
  -> App :~> Handler
appToHandler config logger = Nat appToHandler'
  where
    appToHandler' :: forall a. App a -> Handler a
    appToHandler' app
      = flip runLoggingT logger
      $ flip runReaderT config
      $ unApp app

app
  :: Logger
  -> AppConfig
  -> Application
app logger config =
  let actualApp = flip enter server $ appToHandler config logger
  in serve entireApi $ actualApp
  :<|> getSwaggerData
  :<|> serveDirectory "./static/swagger-ui/dist"
  :<|> serveDirectory "./static/images"
  :<|> serveDirectory "./static/css"
  :<|> serveDirectory "./static/lib"

type WholeAPI = API
  :<|> "swagger.json" :> Get '[JSON] Swagger
  :<|> "swagger" :> Raw
  :<|> "images" :> Raw
  :<|> "css" :> Raw
  :<|> "lib" :> Raw

entireApi :: Proxy WholeAPI
entireApi = Proxy

api :: Proxy API
api = Proxy

getSwaggerData
  :: Handler Swagger
getSwaggerData =
  pure $ toSwagger api
      & info.title .~ "View together api"
