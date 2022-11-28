module Main
  ( main
  )
  where

import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import OperationalWidgets.App (app, Page(..), SharedCardReference, doTestLogin)
import Web.HTML (window)
import Web.HTML.Location (hash)
import Web.HTML.Window (location)

import Control.Bind (bind, discard)
import Effect.Class.Console (log)

main :: Effect Unit
main = do
  w <- window
  l <- location w
  h <- hash l
  log h
  case h of
    -- "#share" -> do
      -- token <-
      -- runWidgetInDom "app" (app (Share (Just token)))
    "#registration" -> runWidgetInDom "app" (app Signup)
    "#login" -> do
      runWidgetInDom "app" (doTestLogin "joe" "clipperz")
    _ -> runWidgetInDom "app" (app (Loading (Just Login)))
