{-# LANGUAGE TemplateHaskell #-}
module Service.Session where

import Import
import Control.Lens
import JavaScript.JQuery

getAvailableSessions
  :: (MonadReader FrontendConfig m, MonadIO m, MonadLogger m, Monad m)
  => m [Session]
getAvailableSessions = do
  uri <- (<> "/v1/session") <$> asks basePath
  res <- liftIO $ ajax uri [] def
  $logInfo $ "Response from server: " <> tshow res
  pure []
