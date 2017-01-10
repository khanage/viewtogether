module ExternalImport
  ( module Import
  ) where

-- This module is here to be referenced by
-- files that still need a prelude, but would
-- otherwise be available within the current project
-- i.e. to avoid circular references

import ClassyPrelude.Conduit as Import
import Control.Monad.Reader.Class as Import
