{-# LANGUAGE CPP #-}
module Import.NoFoundation
    ( module Import
    , isNothingE, onE
    ) where

#if MIN_VERSION_classy_prelude(1,0,0)
import ClassyPrelude.Yesod   as Import hiding (Handler)
#else
import ClassyPrelude.Yesod   as Import hiding
  ( delete, update
  , count, groupBy, selectSource
  , (||.)
  , (>=.), (<=.)
  , (>.), (<.)
  , (==.), (=.), (!=.)
  , (/=.), (-=.), (+=.), (*=.)
  )
#endif

import Model                 as Import
import Settings              as Import
import Settings.StaticFiles  as Import
import Yesod.Auth            as Import
import Yesod.Core.Types      as Import (loggerSet)
import Yesod.Default.Config2 as Import
import Database.Esqueleto as Import hiding (isNothing, on, Value)
import qualified Database.Esqueleto as E (isNothing, on)

isNothingE = E.isNothing
onE = E.on
