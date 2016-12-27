module Handler.Session where

import Import

getSessionR
  :: Handler (Jason [Session])
getSessionR = do
  userId <- requireAuthId

  $logDebug $ "Loading sessions for " <> tshow userId

  sessions <- runDB $ select
                    $ from $ \(session `InnerJoin` userGroup `InnerJoin` groupMembership `InnerJoin` user) -> do
                                   onE (session ^. SessionDbGroup ==. userGroup ^. GroupDbId)
                                   onE (groupMembership ^. GroupMembershipDbGroup ==. userGroup ^. GroupDbId)
                                   onE (groupMembership ^. GroupMembershipDbUser ==. user ^. UserDbId)
                                   where_ (user ^. UserDbId ==. val userId)
                                   return session
  pure $ Jason $ asSession <$> sessions

postSessionR
  :: Handler RepJson
postSessionR = do
  userId <- requireAuthId
  sessionJson <- requireJsonBody :: Handler IncomingSession

  $logDebug $ "Received request for new form"
  currentTime <- liftIO $ getCurrentTime

  $logDebug $ "Creating new session: " <> tshow sessionJson
  showId <- runDB $ insert $ ShowDb {showDbCreated = currentTime, showDbTitle = "Randomly generated", showDbLink = "http://netflix.com" }
  sessionId <- runDB $ insert $ fromIncomingSession sessionJson userId currentTime showId

  redirect $ SpecificSessionR sessionId
  where
    fromIncomingSession IncomingSession{..} userId time showId =
      SessionDb
      { sessionDbBegin = Nothing
      , sessionDbEnd = Nothing
      , sessionDbCreator = userId
      , sessionDbShow = showId
      , sessionDbGroup = toSqlKey isessGroupId
      , sessionDbCreated = time
      , sessionDbTitle = isessTitle
      }

getSpecificSessionR
  :: SessionDbId
  -> Handler (Jason Session)
getSpecificSessionR sessionDbId = do
  userId <- requireAuthId

  $logDebug $ "Loading sessions for " <> tshow userId

  sessions <- runDB $ select
                    $ from $ \(session `InnerJoin` userGroup `InnerJoin` groupMembership `InnerJoin` user) -> do
                                   onE (session ^. SessionDbGroup ==. userGroup ^. GroupDbId)
                                   onE (groupMembership ^. GroupMembershipDbGroup ==. userGroup ^. GroupDbId)
                                   onE (groupMembership ^. GroupMembershipDbUser ==. user ^. UserDbId)
                                   where_ (user ^. UserDbId ==. val userId)
                                   where_ (session ^. SessionDbId ==. val sessionDbId)
                                   return session
  let allSessions = asSession <$> sessions
  case maybeHead allSessions of
    Just session -> pure $ Jason session
    Nothing -> notFound

asSession :: Entity SessionDb -> Session
asSession sessionEntity =
  let SessionDb {..} = entityVal sessionEntity
  in Session { sessId = fromSqlKey (entityKey sessionEntity)
             , sessTitle = sessionDbTitle
             , sessCreated = sessionDbCreated
             , sessGroupId = fromSqlKey sessionDbGroup
             }

putSpecificSessionR
  :: SessionDbId
  -> Handler Html
putSpecificSessionR sessionId = do
  _ <- requireAuthId
  _ <- runDB $ get404 sessionId

  redirect $ SpecificSessionR sessionId
