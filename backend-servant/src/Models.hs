module Models where

import           BaseImports
import           Config
import           Data.Aeson.TH

data User = User
  { userId        :: Int
  , userFirstName :: Text
  , userLastName  :: Text
  } deriving (Eq, Show, Generic)
instance ToSchema User

$(deriveJSON defaultOptions ''User)

data CreateUserResult
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
