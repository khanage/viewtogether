{-# LANGUAGE TemplateHaskell #-}
module Models.Session where

import SharedImport
import Data.Aeson.TH

data IncomingSession = IncomingSession
  { isessTitle :: Text
  , isessGroupId :: Int64
  } deriving (Eq, Show)

data Session = Session
  { sessId :: Int64
  , sessTitle :: Text
  , sessGroupId :: Int64
  , sessCreated :: UTCTime
  } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''IncomingSession)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Session)
