{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( console_log
    , onBoot
    ) where

import ReactWrapper
import Import
import Data.JSString (JSString)
import GHCJS.Types
import GHCJS.DOM (currentDocument)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.Document (getBody, getElementById)
import GHCJS.Marshal (FromJSVal, ToJSVal)
import Config.Strings
import Service.Session (getAvailableSessions)
import qualified React as R
import qualified React.DOM as DOM

foreign import javascript unsafe "console.log($1)" console_log :: JSString -> IO ()

consoleLog
  :: (JsStringConvert s, MonadIO m)
  => s
  -> m ()
consoleLog =
  liftIO . console_log . toJsString

data Counter = Counter { dummy :: String, count :: Int } deriving (Generic)

instance ToJSVal Counter
instance FromJSVal Counter

R.makeClass "counter"
  [| R.statefulSpec (pure $ Counter "" 0) $ do
      clicker <- R.eventHandler . const $ R.getState >>= R.setState . Counter "" . (+1) . count
      pure $ do
        count <- count <$> R.getState
        this <- ask
        pure $
          DOM.div_ []
            [ DOM.div_ [] ["The count is ", R.text $ show count]
            , DOM.button_ [R.onClick this clicker] ["Click!"]
            ]
  |]
counter :: R.ReactClass R.OnlyAttributes

root :: R.ReactNode
root =
  DOM.div_ []
    [ DOM.h1_ [] ["hello world from react"]
    , R.createElement counter [] []
    ]

onBoot
  :: IO ()
onBoot = do
  consoleLog $ "Hello from custom GHCJS! " <> someMessage
  printLn "Booyah"
  Just doc <- currentDocument
  Just body <- getBody doc
  Just appNode <- getElementById doc ("application" :: JSString)
  R.render root appNode Nothing
  let config = FrontendConfig
               { basePath = "http://192.168.59.103:3000"
               }
  printLn $ tshow config
