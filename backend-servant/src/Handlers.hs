{-# LANGUAGE FlexibleContexts #-}
module Handlers where

import           DbModels
import           Imports
import Database.Esqueleto
import Servant
import User

rootHandler :: App Text
rootHandler = do
  $logInfo $ "Landed on the home page"
  pure "hello"

getUsersHandler :: App [User]
getUsersHandler = do
  $logDebug $ "Loading users"
  dbUsers :: [Entity DbUser] <- runDb
    $ select
    $ from $ \user ->
               do pure user
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
    AlreadyExists -> duplicateUser

  where
    redirectTo uri =
      throwError $ err302 { errHeaders = [("Location", uri)]}
    duplicateUser =
      let message = "User with name '"
            <> tshow (ueIdent userToCreate)
            <> "' already exists."
      in throwError
         $ err400 { errReasonPhrase = tunpack message }

asUser
  :: Entity DbUser
  -> User
asUser euser =
  let dbUser = entityVal euser

      uid = fromIntegral $ fromSqlKey $ entityKey euser
      first = dbUserFirstName dbUser
      last = dbUserLastName dbUser

  in User uid first last
