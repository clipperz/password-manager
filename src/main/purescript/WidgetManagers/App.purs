module WidgetManagers.App where

import Concur.Core (Widget)
import Control.Monad.State (runStateT, get)
import Concur.React (HTML)
import Control.Bind (bind, discard)
import Data.Function (($), flip)
import Data.Functor (void)
import Data.Unit (Unit)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.SRP as SRP
import Functions.State (computeInitialState)
import Functions.JSState (updateAppState)
import WidgetManagers.HomePageManager as HomePageManager
import WidgetManagers.LandingPageManager as LandingPageManager

import Data.Show (show)
import Data.Semigroup ((<>))
import Effect.Class.Console (log)

app :: Widget HTML Unit
app = do
  initialState <- liftEffect computeInitialState
  liftAff $ updateAppState initialState
  _ <- do
    {- indexReference <- -} LandingPageManager.landingPage SRP.baseConfiguration
    -- void $ HomePageManager.homePageManager indexReference
  app
