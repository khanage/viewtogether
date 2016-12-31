module Config where

import           BaseImports
import           Control.Monad.Except
import           Control.Monad.Logger     (LoggingT, MonadLogger, MonadLoggerIO)
import           Data.Pool                (Pool)
import           Database.Persist.Sql     (SqlBackend)
import           Network.Wai.Handler.Warp (Port)
import           Servant.Server           (ServantErr)

newtype App a = App
  { unApp :: ReaderT AppConfig (LoggingT (ExceptT ServantErr IO)) a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadLogger
    , MonadLoggerIO
    , MonadReader AppConfig
    , MonadError ServantErr
    )

data AppConfig = AppConfig
  { acPool         :: Pool SqlBackend
  , acPort         :: Port
  , acJwtSecretKey :: ByteString
  }
