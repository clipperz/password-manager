module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import WidgetManagers.App (app)

main :: Effect Unit
main = do
  runWidgetInDom "app" app
