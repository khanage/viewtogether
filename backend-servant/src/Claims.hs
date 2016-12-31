module Claims where

import qualified Data.Aeson            as Aeson
import           Imports
import           Jose.Jwa
import           Jose.Jws
import           Jose.Jwt
import           Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import           Servant.Server        (BasicAuthCheck (BasicAuthCheck), BasicAuthResult (Authorized, Unauthorized))


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
       Right (Jwt encoded) -> pure $ GrantedClaims $ fromUtf8 encoded

asClaims
  :: [(Text,Text)]
  -> ByteString
asClaims = toStrict . Aeson.encode

