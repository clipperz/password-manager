module WidgetManagers.App where

import Concur.Core (Widget)
import Control.Monad.State (runStateT, get)
import Concur.React (HTML)
import Control.Bind (bind)
import Data.Function (($), flip)
import Data.Functor (void)
import Data.Unit (Unit)
import Effect.Class (liftEffect)
import Functions.SRP as SRP
import Functions.State(computeInitialState)
import WidgetManagers.HomePageManager as HomePageManager
import WidgetManagers.LandingPage as LandingPage

import Data.Show (show)
import Data.Semigroup ((<>))
import Effect.Class.Console (log)

app :: Widget HTML Unit
app = do
  initialState <- liftEffect computeInitialState
  _ <- (flip runStateT) initialState $ do
    indexReference <- LandingPage.landingPage SRP.baseConfiguration
    newState <- get
    _ <- log $ "newState (app) -> " <> show newState
    void $ HomePageManager.homePageManager indexReference
  app