module OperationalWidgets.App where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Functor (void)
import Data.Unit (Unit)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.State (computeInitialState)
import Functions.JSState (modifyAppState)
import OperationalWidgets.HomePageWidget as HomePageWidget
import Views.LoginFormView (emptyForm)
import Views.LandingPageView (landingPageView, LandingPageView(..))

import Data.Show (show)
import Effect.Class.Console (log)
import Functions.Import (decodeImport)
import Views.SimpleWebComponents (textAreaWidget)

app :: Widget HTML Unit
app = do
  -- initialState <- liftEffect computeInitialState
  -- liftAff $ modifyAppState initialState
  -- _ <- do
  --   indexReference <- landingPageView (LoginView Default emptyForm)
  --   void $ HomePageWidget.homePageWidget indexReference
  -- app
  s <- textAreaWidget "" "Submit"
  log $ show $ decodeImport s
