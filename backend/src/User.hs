{-# LANGUAGE RecordWildCards #-}
module User where

import Imports
import DbModels
import Database.Esqueleto

createNewUser
  :: UserEdit
  -> App CreateResult
createNewUser UserEdit{..} = do
  alreadyExists <- fmap (not . null) $ runDb
    $ select
    $ from $ \user ->
      do where_ (user ^. DbUserIdent ==. val ueIdent)
         pure user

  if alreadyExists
    then pure AlreadyExists
    else do
      let newUser = DbUser ueIdent ueFirst ueLast (Just uePass)
          forceInt = fromIntegral . fromSqlKey
      key <- fmap forceInt $ runDb $ insert $ newUser
      pure $ Created key
