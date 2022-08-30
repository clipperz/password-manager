module WidgetManagers.App where

import Concur.Core (Widget)
import Control.Applicative (pure)
import Control.Monad.State (runStateT)
import Concur.React (HTML)
import Control.Bind (bind)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.AppState (AppState)
import DataModel.Proxy (Proxy(..))
import Effect (Effect)
import Effect.Class (liftEffect)
import SRP as SRP
import WidgetManagers.HomePageManager as HomePageManager
import WidgetManagers.LandingPage as LandingPage

computeInitialState :: Effect AppState
computeInitialState = pure { proxy: (OnlineProxy ""), sessionKey: Nothing, toll: Nothing, c: Nothing, p: Nothing }

app :: Widget HTML Unit
app = do
  initialState <- liftEffect computeInitialState
  Tuple loginResult newState <- runStateT (LandingPage.landingPage SRP.baseConfiguration) initialState
  -- void $ runStateT (homePageManager loginResult) newState
  HomePageManager.homePageManager loginResult
