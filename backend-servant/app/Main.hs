module Main where

import           ClassyPrelude
import           Config                  (AppConfig (..))
import           Control.Monad.Logger    (logDebug, logInfo)
import           Control.Monad.Logger    (runStdoutLoggingT)
import           Database.Persist.Sqlite ( createSqlitePool
                                         , runMigration
                                         , runSqlPool)
import           DbModels                (migrateAll)
import           Lib                     (startApp)

main :: IO ()
main = runStdoutLoggingT $ do

  let port = 8080
      connString = "viewtogether.db"

  $logDebug $ "Creating sql pool."
  pool <- createSqlitePool connString 3

  $logDebug $ "Running migrations."
  flip runSqlPool pool $ runMigration migrateAll

  $logInfo $ "Finished booting."
  startApp port $ AppConfig { acPool = pool }
