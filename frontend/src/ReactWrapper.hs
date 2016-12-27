module ReactWrapper
  ( printLn
  ) where

import Import
import Data.JSString as S
import GHCJS.Types

foreign import javascript unsafe "var para = document.createElement('p'); var t = document.createTextNode($1); para.appendChild(t); para.appendChild(document.createElement('br')); document.body.appendChild(para)" js_println :: S.JSString -> IO()

printLn
  :: Text
  -> IO ()
printLn =
  js_println . toJsString
