{-# LANGUAGE EmptyDataDecls, FlexibleContexts, GADTs,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses,
             OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies #-}
module DbModels where

import           BaseImports
import           Config
import           Database.Esqueleto
import           Database.Persist.Sql
import           Database.Persist.TH
import           Models

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DbUser
  ident UserIdent
  firstName Text
  lastName Text
  password Password Maybe
  UniqueUser ident
  deriving Typeable
DbGroup
  name Text
  created UTCTime
  UniqueGroupName name
  deriving Eq
  deriving Show
DbGroupMembership
  user DbUserId
  group DbGroupId
  admin Bool
  deriving Eq
  deriving Show

DbSession
  title Text
  created UTCTime
  begin UTCTime Maybe
  end UTCTime Maybe
  group DbGroupId
  creator DbUserId
  show DbShowId
  deriving Eq
  deriving Show
DbShow
  created UTCTime
  title Text
  link Text
  deriving Eq
  deriving Show
DbComment
  created UTCTime
  session DbSessionId
  content Text
  eventTime UTCTime Maybe
  user DbUserId
  deriving Eq
  deriving Show
|]

runDb
  :: SqlPersistT IO b
  -> App b
runDb action = do
  pool <- asks acPool
  liftIO $ runSqlPool action pool
