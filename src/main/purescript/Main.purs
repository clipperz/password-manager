module Main where

import Prelude

import Concur.React.Run (runWidgetInDom)
import Effect (Effect)
import OperationalWidgets.App (app)

import Control.Bind ((>>=))
import Data.Functor ((<$>))
import Data.Show (show)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.JSState (updateAppState, getAppState)
import Functions.State(computeInitialState)

main :: Effect Unit
main = do
  runWidgetInDom "app" app
