module WidgetManagers.App where

import Concur.Core (Widget)
import Control.Monad.State (runStateT, get)
import Concur.React (HTML)
import Control.Bind (bind, discard)
import Data.Function (($), flip)
import Data.Functor (void)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect.Class (liftEffect)
import Functions.State(computeInitialState)
import SRP as SRP
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

  -- Tuple indexReference newState <- runStateT (LandingPage.landingPage SRP.baseConfiguration) initialState
  -- void $ runStateT (HomePageManager.homePageManager indexReference) newState
  app