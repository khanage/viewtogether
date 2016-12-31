module Session where

import Imports
import DbModels
import Database.Esqueleto

createNewSession
  :: SessionEdit
  -> App CreateResult
createNewSession SessionEdit{..} = do
  alreadyExists <- fmap (not . null) $ runDb
    $ select
    $ from $ \session ->
      do where_ (session ^. DbSessionTitle ==. val seTitle)
         pure session

  if alreadyExists
    then pure AlreadyExists
    else do
      currentTime <- liftIO getCurrentTime
      let
        groupId = toSqlKey seGroupId
        userId = toSqlKey seUserId
        showId = toSqlKey seShowId
        newSession = DbSession seTitle currentTime seBegin seEnd groupId userId showId
        forceInt = fromIntegral . fromSqlKey

      key <- fmap forceInt $ runDb $ insert $ newSession
      pure $ Created key

