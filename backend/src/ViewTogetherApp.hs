{-# LANGUAGE DataKinds, FlexibleInstances, TemplateHaskell, TypeOperators #-}
module ViewTogetherApp (startApp) where

import           Claims
import           Control.Category
import           Control.Lens                     ((%~), (&), (.~), (<&>))
import           Control.Monad.Except
import           Data.Aeson
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy             as BSL
import           Data.Swagger
import           Handlers
import           Imports
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           SwaggerHelp

import           Servant.API.BasicAuth            (BasicAuthData (BasicAuthData))
import           Servant.Server                   (BasicAuthCheck (BasicAuthCheck),
                                                   BasicAuthResult (Authorized, Unauthorized),
                                                   Context ((:.), EmptyContext),
                                                   Handler, Server, err401,
                                                   err403, errBody,
                                                   serveWithContext)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           Servant.Server.Experimental.Auth ()

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

privateThingHandler
  :: GrantedClaims
  -> App Text
privateThingHandler claims =
  pure "Secret of space"

type instance AuthServerData (AuthProtect "claims") = GrantedClaims

type API = Get '[JSON] Text
  :<|> "grant-claims" :> BasicAuth "viewtogether" GrantedClaims
                      :> Get '[JSON] Text
  :<|> AuthProtect "claims" :> "private" :> "thing" :> Get '[JSON] Text
  :<|> AuthProtect "claims" :> "users" :> UsersAPI
  :<|> AuthProtect "claims" :> "sessions" :> SessionAPI

type UsersAPI = Get '[JSON] [User]
  :<|> ReqBody '[JSON] UserEdit :> Post '[JSON] ()

userHandlers
  :: GrantedClaims
  -> App [User] :<|> (UserEdit -> App ())
userHandlers claims =
  getUsersHandler claims :<|> postUserHandler claims

type SessionAPI = Get '[JSON] [Session]
  :<|> ReqBody '[JSON] SessionEdit :> Post '[JSON] ()

sessionHandlers
  :: GrantedClaims
  -> App [Session] :<|> (SessionEdit -> App ())
sessionHandlers claims =
  getSessionsHandler claims :<|> postSessionHandler claims

type WholeAPI = API -- :<|> SwaggerAPI

server :: AppServer API
server = rootHandler
  :<|> grantClaimsHandler
  :<|> privateThingHandler
  :<|> userHandlers
  :<|> sessionHandlers

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
      runToHandler = appToHandler config logger
      runToIo = appToIo runToHandler
      context = serverContext runToHandler runToIo
  in serveWithContext entireApi context actualApp -- (actualApp :<|> swaggerServer api)

entireApi :: Proxy WholeAPI
entireApi = Proxy

api :: Proxy API
api = Proxy
