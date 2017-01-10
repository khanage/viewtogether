module Main where

import           ClassyPrelude
import           Control.Monad.Logger     (runStdoutLoggingT)
import           EntryPoint               (runner)

main :: IO ()
main = runStdoutLoggingT $ do

  let port = 8080
      connString = "viewtogether.db"

  runner port port connString
