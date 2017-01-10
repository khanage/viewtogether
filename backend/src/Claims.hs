module Claims where

import Data.Swagger
import           Control.Lens
import qualified Data.Aeson                       as Aeson
import           Imports
import           Jose.Jwa
import           Jose.Jws
import           Jose.Jwt
import           Network.Wai
import           Servant
import           Servant.API.BasicAuth            (BasicAuthData (BasicAuthData))
import           Servant.Server                   (BasicAuthCheck (BasicAuthCheck),
                                                   BasicAuthResult (Authorized, Unauthorized))
import           Servant.Server                   (Handler)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)

grantClaims
  :: BasicAuthData
  -> App (BasicAuthResult GrantedClaims)
grantClaims (BasicAuthData username password) =
  do loginResult <- validateUserFromDb username password
     if loginResult
       then Authorized <$> generateClaimsFor
       else pure Unauthorized

validateUserFromDb
  :: ByteString
  -> ByteString
  -> App Bool
validateUserFromDb u p =
  pure True

generateClaimsFor
  :: App GrantedClaims
generateClaimsFor =
  do secret <- asks acJwtSecretKey
     case hmacEncode HS256 secret $ asClaims $ [("authed", tshow True)] of
       Left err -> fail $ show err
       Right encoded -> pure $ GrantedClaims $ encoded

claimsAuthHandler
  :: App :~> Handler
  -> AuthHandler Request GrantedClaims
claimsAuthHandler runToHandler =
  mkAuthHandler $ \req -> enter runToHandler $
      do let q = join . lookup "claims" . queryString
             h = lookup "claims" . requestHeaders
             runAll = foldr ((<|>) . (&) req) empty
         case runAll [q, h] of
           Just claim ->
             do secretKey <- asks acJwtSecretKey
                case hmacDecode secretKey claim of
                  Left err -> do $logInfo $ "Bad signature resulted in " <> tshow err
                                 throwError $ err401 { errBody = "Missing claims"}
                  Right jws -> do $logInfo $ "Got claims all like " <> tshow jws
                                  pure $ GrantedClaims $ Jwt claim
           Nothing -> throwError $ err401 { errBody = "Missing claims"}

asClaims
  :: [(Text,Text)]
  -> ByteString
asClaims = toStrict . Aeson.encode
