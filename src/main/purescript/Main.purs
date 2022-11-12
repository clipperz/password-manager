module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import OperationalWidgets.App (app, app', Page(..))

main :: Effect Unit
main = runWidgetInDom "app" (app Login)

registration :: Effect Unit
registration = runWidgetInDom "app" (app Signup)

share :: String -> Effect Unit
share token = runWidgetInDom "app" (app (Share token))


