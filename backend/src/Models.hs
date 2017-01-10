module Models where

import           BaseImports
import           Config
import           Data.Aeson             
import           Data.Aeson.TH
import           Database.Persist.Sql   (PersistField (..),
                                         PersistFieldSql (..))
import           Database.Persist.Types (SqlType (..))
import           Jose.Jwa
import           Jose.Jws
import           Jose.Jwt

newtype Password = Password { unPassword :: Text }
  deriving (IsString, Show, Generic)
instance PersistField Password where
  toPersistValue = toPersistValue . unPassword
  fromPersistValue = fmap Password . fromPersistValue
instance PersistFieldSql Password where
  sqlType = const SqlString
instance ToSchema Password

$(deriveJSON defaultOptions ''Password)
{-
 This doesn't interact nicely with good aeson
instances
-}
-- $(deriveJSON defaultOptions{ unwrapUnaryRecords = True
--                            , fieldLabelModifier = drop 2
--                            , constructorTagModifier = toLower
--                            } ''Password)


newtype UserIdent = UserIdent { unIdent :: Text }
  deriving (IsString, Show, Generic)
instance PersistField UserIdent where
  toPersistValue = toPersistValue . unIdent
  fromPersistValue = fmap UserIdent . fromPersistValue
instance PersistFieldSql UserIdent where
  sqlType = const SqlString
instance ToSchema UserIdent

$(deriveJSON defaultOptions ''UserIdent)
-- $(deriveJSON defaultOptions{ unwrapUnaryRecords = True
--                            , fieldLabelModifier = drop 2
--                            , constructorTagModifier = toLower
--                            } ''UserIdent)


data User = User
  { userId        :: Int64
  , userFirstName :: Text
  , userLastName  :: Text
  } deriving (Eq, Show, Generic)
instance ToSchema User

$(deriveJSON defaultOptions ''User)

data CreateResult
  = Created Int
  | AlreadyExists

data UserEdit = UserEdit
  { ueIdent :: UserIdent
  , ueFirst :: Text
  , ueLast  :: Text
  , uePass  :: Password
  } deriving (Show, Generic)
instance ToSchema UserEdit

$(deriveJSON defaultOptions ''UserEdit)

data Session = Session
  { ssId      :: Int64
  , ssTitle   :: Text
  , ssCreated :: UTCTime
  , ssBegin   :: Maybe UTCTime
  , ssEnd     :: Maybe UTCTime
  , ssGroupId :: Int64
  , ssUserId  :: Int64
  } deriving (Generic, Show)
instance ToSchema Session

$(deriveJSON defaultOptions ''Session)

data SessionEdit = SessionEdit
  { seTitle   :: Text
  , seBegin   :: Maybe UTCTime
  , seEnd     :: Maybe UTCTime
  , seGroupId :: Int64
  , seUserId  :: Int64
  , seShowId  :: Int64
  } deriving (Show, Generic)
instance ToSchema SessionEdit

$(deriveJSON defaultOptions ''SessionEdit)

data GrantedClaims = GrantedClaims
  { jwt :: Jwt
  } deriving (Eq, Show) -- No generic as all the things should be handrolled

instance ToJSON GrantedClaims where
  toJSON (GrantedClaims (Jwt payload)) =
    object ["claims" .= fromUtf8 payload]

  toEncoding (GrantedClaims (Jwt payload)) =
    pairs ("claims" .= fromUtf8 payload)

-- $(deriveJSON defaultOptions ''GrantedClaims)

