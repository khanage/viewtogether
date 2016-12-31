module EntryPoint
  ( runner
  , develMain
  ) where

import           Control.Lens
import           Control.Monad.Logger     (LoggingT, MonadLogger (..), logDebug,
                                           logInfo, runStdoutLoggingT)
import           Database.Persist.Sql
import           Database.Persist.Sqlite  (createSqlitePool, runMigration,
                                           runSqlPool)
import           DbModels                 (migrateAll)
import           Imports
import           ViewTogetherApp                      (startApp)
import           Network.Wai.Handler.Warp (Port)
import           System.Directory         (doesFileExist)
import           System.Environment       (lookupEnv)

runner
  :: Port
  -> Port
  -> ByteString
  -> Text
  -> LoggingT IO ()
runner port publicPort secretKey connectionString = do
  pool <- createPoolAndRunMigrations connectionString
  startApp publicPort
         $ AppConfig { acPool = pool
                     , acPort = port
                     , acJwtSecretKey = secretKey
                     }

createPoolAndRunMigrations
  :: (MonadLogger m, MonadIO m, MonadBaseControl IO m)
  => Text
  -> m ConnectionPool
createPoolAndRunMigrations connString = do
  $logDebug $ "Creating sql pool."
  pool <- createSqlitePool connString 3

  $logDebug $ "Running migrations."
  flip runSqlPool pool $ runMigration migrateAll

  pure pool

{- Yesod dev support stuff below -}
develMain :: IO ()
develMain = race_ watchTermFile $ do
  publicPort <- lookupAt "PORT"
  port <- lookupAt "DISPLAY_PORT"
  secretKey <- pure "SECRET_KEY"
  runStdoutLoggingT $ runner port publicPort secretKey "viewtogether.db"
  where
    lookupAt envVar =
      let errMsg = "no " <> envVar <> " env variable"
      in lookupEnv envVar & liftRead & (=<<) (maybe (fail errMsg) pure)
    liftRead :: Read a => IO (Maybe String) -> IO (Maybe a)
    liftRead = fmap (>>= readMay)

watchTermFile :: IO ()
watchTermFile =
  let loop = do exists <- doesFileExist "yesod-devel/devel-terminate"
                if exists
                  then pure ()
                  else threadDelay 100000 >> loop
  in loop
