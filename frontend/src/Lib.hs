{-# LANGUAGE DeriveGeneric, OverloadedStrings, TemplateHaskell #-}
module Lib
    ( console_log
    , onBoot
    ) where

import           Data.JSString      (JSString)
import qualified Data.JSString      as S
import           GHCJS.DOM          (currentDocument)
import           GHCJS.DOM.Document (getBody, getElementById)
import           GHCJS.DOM.Node     (appendChild)
import           GHCJS.Marshal      (FromJSVal(..), ToJSVal(..))
import GHCJS.Marshal.Pure (pToJSVal)
import           GHCJS.Types
import           Import
import qualified React              as R
import qualified React.DOM          as DOM
import           ReactWrapper
import           Service.Session    (getAvailableSessions)

foreign import javascript unsafe "console.log($1)" console_log :: JSString -> IO ()

consoleLog
  :: (MonadIO m)
  => Text
  -> m ()
consoleLog =
  liftIO . console_log . toJsString

packText
  :: Text
  -> JSVal
packText = pToJSVal

data Counter = Counter { dummy :: String, count :: Int } deriving (Generic)

instance ToJSVal Counter
instance FromJSVal Counter

test = _

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

R.makeClass "login"
  [| R.statefulSpec (pure $ Counter "" 0) $ do
      pure . pure $
        DOM.div_ []
        [ DOM.div_ [R.Prop "id" (packText "1q23")] ["Yeah, nah, login mate!"]
        ]
  |]
login :: R.ReactClass R.OnlyAttributes

root :: R.ReactNode
root =
  DOM.div_ []
    [ DOM.h1_ [] ["Welcome to view together"]
    , DOM.h2_ [] ["Experience together"]
    , R.createElement counter [] []
    , R.createElement login [] []
    ]

onBoot
  :: IO ()
onBoot = do
  consoleLog $ "Hello from custom GHCJS!"
  Just doc <- currentDocument
  Just body <- getBody doc
  Just appNode <- getElementById doc ("application" :: JSString)
  R.render root appNode Nothing
  let config = FrontendConfig
               { basePath = "http://localhost:3000"
               }

  printLn $ tshow config
