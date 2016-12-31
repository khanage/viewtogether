{-# LANGUAGE NoMonomorphismRestriction #-}
module Handlers where

import           Database.Esqueleto
import           DbModels
import           Imports
import           Servant
import           User
import Session

rootHandler :: App Text
rootHandler = do
  $logInfo $ "Landed on the home page"
  pure "hello"

getUsersHandler :: App [User]
getUsersHandler = do
  $logDebug $ "Loading users"
  dbUsers :: [Entity DbUser] <- runDb
    $ select
    $ from pure
  pure $ asUser <$> dbUsers

postUserHandler
  :: UserEdit
  -> App ()
postUserHandler userToCreate = do
  $logInfo $ "Creating a new user for identifier "
    <> tshow (ueIdent userToCreate)

  createResult <- createNewUser userToCreate

  case createResult of
    Created id -> redirectTo $ "/users/" <> utf8show id
    AlreadyExists -> duplicate

  where
    redirectTo uri =
      throwError $ err302 { errHeaders = [("Location", uri)]}
    duplicate =
      let message = "User with name '"
            <> tshow (ueIdent userToCreate)
            <> "' already exists."
      in throwError
         $ err400 { errReasonPhrase = tunpack message }

getSessionsHandler :: App [Session]
getSessionsHandler = do
  $logDebug $ "Loading sessions"
  dbSession :: [Entity DbSession] <- runDb
    $ select
    $ from pure
  pure $ asSession <$> dbSession

postSessionHandler
  :: SessionEdit
  -> App ()
postSessionHandler sessionToCreate = do
  $logInfo $ "Creating a new session for identifier "
    <> tshow (seTitle sessionToCreate)

  createResult <- createNewSession sessionToCreate

  case createResult of
    Created id -> redirectTo $ "/session/" <> utf8show id
    AlreadyExists -> duplicate

  where
    redirectTo uri =
      throwError $ err302 { errHeaders = [("Location", uri)]}
    duplicate =
      let message = "Session with name '"
            <> tshow (seTitle sessionToCreate)
            <> "' already exists."
      in throwError
         $ err400 { errReasonPhrase = tunpack message }

grantClaimsHandler
  :: GrantedClaims
  -> App GrantedClaims
grantClaimsHandler =
  pure

{- Mapping -}
asUser
  :: Entity DbUser
  -> User
asUser euser =
  let dbUser = entityVal euser

      uid = forceIntKey $ entityKey euser
      first = dbUserFirstName dbUser
      last = dbUserLastName dbUser

  in User uid first last

asSession
  :: Entity DbSession
  -> Session
asSession esession =
  let DbSession{..} = entityVal esession
      ssId = forceIntKey $ entityKey esession

      ssTitle = dbSessionTitle
      ssCreated = dbSessionCreated
      ssBegin = dbSessionBegin
      ssEnd = dbSessionEnd
      ssGroupId = forceIntKey $ dbSessionGroup
      ssUserId = forceIntKey $ dbSessionCreator

  in Session{..}



{- Util -}
forceIntKey
  :: (ToBackendKey SqlBackend record)
  => Key record
  -> Int64
forceIntKey =
  fromIntegral . fromSqlKey
