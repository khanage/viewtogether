module BaseImports
  ( module X

  , tunpack
  , utf8show

  , toUtf8
  , fromUtf8
  ) where

import           ClassyPrelude          as X
import           Data.Swagger           as X (ToSchema (..))
import           Data.Time              as X
import           Data.Typeable          as X
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE

tunpack
  :: Text
  -> String
tunpack =
  T.unpack

utf8show
  :: Show a
  => a
  -> ByteString
utf8show =
  TE.encodeUtf8 . tshow

toUtf8
  :: Text
  -> ByteString
toUtf8 =
  TE.encodeUtf8

fromUtf8
  :: ByteString
  -> Text
fromUtf8 =
  TE.decodeUtf8
