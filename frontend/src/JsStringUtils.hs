{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JsStringUtils where

import ExternalImport
import qualified Data.JSString as S
import qualified Data.Text as T

class JsStringConvert s where
  toJsString :: s -> S.JSString
  fromJsString :: S.JSString -> s

instance JsStringConvert String where
  toJsString = S.pack
  fromJsString = S.unpack

instance JsStringConvert T.Text where
  toJsString = S.pack . T.unpack
  fromJsString = T.pack . S.unpack
