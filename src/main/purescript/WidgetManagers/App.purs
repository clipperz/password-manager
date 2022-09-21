module WidgetManagers.App where

import Concur.Core (Widget)
import Control.Monad.State (runStateT)
import Concur.React (HTML)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Functor (void)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect.Class (liftEffect)
import Functions.State(computeInitialState)
import SRP as SRP
import WidgetManagers.HomePageManager as HomePageManager
import WidgetManagers.LandingPage as LandingPage

app :: Widget HTML Unit
app = do
  initialState <- liftEffect computeInitialState
  Tuple indexReference newState <- runStateT (LandingPage.landingPage SRP.baseConfiguration) initialState
  void $ runStateT (HomePageManager.homePageManager indexReference) newState
  app