{-# LANGUAGE DataKinds, FlexibleInstances, TemplateHaskell, TypeOperators #-}
module ViewTogetherApp (startApp) where

import           Control.Lens ((&), (.~), (%~), (<&>))
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy     as BSL
import           Data.Swagger
import           Handlers
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Swagger          (toSwagger, HasSwagger(..))
import           Imports                  hiding (Handler)
import Claims
import Control.Category
import Servant.API.BasicAuth (BasicAuthData(BasicAuthData))
import Servant.Server ( BasicAuthCheck(BasicAuthCheck)
                      , BasicAuthResult (Authorized, Unauthorized)
                      , Context ((:.), EmptyContext)
                      , err401, err403, errBody, Server
                      , serveWithContext, Handler
                      )
import Servant.Server.Experimental.Auth ( AuthHandler
                                        , AuthServerData
                                        , mkAuthHandler)
import Servant.Server.Experimental.Auth ()
import Data.Swagger
import Servant.Swagger


startApp
  :: (MonadLoggerIO m)
  => Port
  -> AppConfig
  -> m ()
startApp port config = do
  $logInfo $ "Running on port " <> tshow port <> "."
  logger <- askLoggerIO
  liftIO $ run port $ app logger config

{- Api definition -}

serverContext
  :: App :~> Handler
  -> App :~> IO
  -> Context (  BasicAuthCheck GrantedClaims
             ': AuthHandler Request GrantedClaims
             ': '[]
             )
serverContext runToHandler runToIo =
  grantClaimsCheck runToIo :. claimsAuthHandler runToHandler :. EmptyContext

claimsAuthHandler
  :: App :~> Handler
  -> AuthHandler Request GrantedClaims
claimsAuthHandler runToHandler =
  mkAuthHandler $ \req -> enter runToHandler $
      do let q = join . lookup "claims" . queryString
             h = lookup "claims" . requestHeaders
             runAll = foldr ((<|>) . (&) req) empty
         case runAll [q, h] of
           Just claim -> pure $ GrantedClaims $ fromUtf8 claim
           Nothing -> throwError $ err401 { errBody = "Missing claims"}

privateThingHandler
  :: GrantedClaims
  -> App Text
privateThingHandler claims =
  pure "Secret of space"

type instance AuthServerData (AuthProtect "claims") = GrantedClaims

type AppServer a = ServerT a App
type API = Get '[JSON] Text
  :<|> "grant-claims" :> BasicAuth "foo-realm" GrantedClaims :> Get '[JSON] GrantedClaims
  :<|> "private" :> "thing" :> AuthProtect "claims" :> Get '[JSON] Text
  :<|> "users" :> UsersAPI
  :<|> "sessions" :> SessionAPI

type UsersAPI = Get '[JSON] [User]
  :<|> ReqBody '[JSON] UserEdit :> Post '[JSON] ()

type SessionAPI = Get '[JSON] [Session]
  :<|> ReqBody '[JSON] SessionEdit :> Post '[JSON] ()

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
  :<|> "swagger" :> Raw
  :<|> "images" :> Raw
  :<|> "css" :> Raw
  :<|> "lib" :> Raw

type WholeAPI = API :<|> SwaggerAPI

server :: AppServer API
server = rootHandler
  :<|> grantClaimsHandler
  :<|> privateThingHandler
  :<|> (getUsersHandler :<|> postUserHandler)
  :<|> (getSessionsHandler :<|> postSessionHandler)

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

appToIo
  :: App :~> Handler
  -> App :~> IO
appToIo f
  = f >>> Nat run
  where
    run :: forall a. Handler a -> IO a
    run = (=<<) (either (fail . show) pure) . runExceptT

grantClaimsCheck
  :: App :~> IO
  -> BasicAuthCheck GrantedClaims
grantClaimsCheck runner =
  BasicAuthCheck $ enter runner . grantClaims

app
  :: Logger
  -> AppConfig
  -> Application
app logger config =
  let actualApp = flip enter server $ appToHandler config logger
      swaggerRelated = getSwaggerData
        :<|> serveDirectory "./static/swagger-ui/dist"
        :<|> serveDirectory "./static/images"
        :<|> serveDirectory "./static/css"
        :<|> serveDirectory "./static/lib"
      runToHandler = appToHandler config logger
      runToIo = appToIo runToHandler
      context = serverContext runToHandler runToIo
  in serveWithContext entireApi context (actualApp :<|> swaggerRelated)

entireApi :: Proxy WholeAPI
entireApi = Proxy

api :: Proxy API
api = Proxy

getSwaggerData
  :: Handler Swagger
getSwaggerData = pure
  $ toSwagger api
  & info.title .~ "View together api"
  & info.description .~ Just "Backing services for all the viewtogether things."

instance (HasSwagger sub, ToParamSchema a)
  => HasSwagger (BasicAuth realm a :> sub) where
  toSwagger _ = toSwagger (Proxy :: Proxy sub)
    & addParam basicAuthHeader
    where
      basicAuthHeader = mempty
        & name .~ "Basic"
        & required .~ Just True
        & description .~ Just "<name:password>"
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
        & name .~ "Basic"
        & required .~ Just True
        & description .~ Just "<name:password>"
        & schema .~ ParamOther (mempty
             & in_ .~ ParamHeader
             & paramSchema .~ toParamSchema (Proxy :: Proxy GrantedClaims))
      addParam :: Param -> Swagger -> Swagger
      addParam param = allOperations.parameters %~ (Inline param :)


instance ToParamSchema GrantedClaims where
instance ToSchema GrantedClaims
