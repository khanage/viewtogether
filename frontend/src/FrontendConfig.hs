module FrontendConfig where

import ExternalImport

data FrontendConfig = FrontendConfig
  { basePath :: Text
  } deriving (Eq, Show)
