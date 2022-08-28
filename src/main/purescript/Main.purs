module Main where

import Concur.React.Run (runWidgetInDom)
import Data.Unit (Unit)
import Effect (Effect)

import WidgetManagers.App (app)

main :: Effect Unit
main = do
  runWidgetInDom "app" app