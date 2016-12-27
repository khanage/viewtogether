module BaseImports
  ( module X

  , Password (..)
  , UserIdent (..)

  , tunpack
  , utf8show
  ) where

import           ClassyPrelude          as X
import           Data.Swagger           as X (ToSchema (..))
import           Data.Time              as X
import           Data.Typeable          as X

import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Database.Persist.Sql   (PersistField (..),
                                         PersistFieldSql (..))
import           Database.Persist.Types (SqlType (..))

newtype Password = Password { unPassword :: Text }
  deriving (IsString, ToJSON, FromJSON, Show, Generic)
instance PersistField Password where
  toPersistValue = toPersistValue . unPassword
  fromPersistValue = fmap Password . fromPersistValue
instance PersistFieldSql Password where
  sqlType = const SqlString
instance ToSchema Password

newtype UserIdent = UserIdent { unIdent :: Text }
  deriving (IsString, ToJSON, FromJSON, Show, Generic)
instance PersistField UserIdent where
  toPersistValue = toPersistValue . unIdent
  fromPersistValue = fmap UserIdent . fromPersistValue
instance PersistFieldSql UserIdent where
  sqlType = const SqlString
instance ToSchema UserIdent

tunpack
  :: Text
  -> String
tunpack =
  T.unpack

utf8show
  :: Show a
  => a
  -> ByteString
utf8show =
  TE.encodeUtf8 . tshow

fromUtf8
  :: ByteString
  -> Text
fromUtf8 =
  TE.decodeUtf8
