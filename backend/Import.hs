module Import
    ( module Import
    , toSqlKey
    , maybeHead
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import
import Models.Session as Import
import Database.Persist.Sql (toSqlKey)

newtype Jason a = Jason a

instance (ToJSON a) => ToContent (Jason a) where
  toContent (Jason a) = toContent $ toJSON a
instance (ToJSON a) => ToTypedContent (Jason a) where
  toTypedContent a = toTypedContent $ repJson a


maybeHead
  :: [a]
  -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x
