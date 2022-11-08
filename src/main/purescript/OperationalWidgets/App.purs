module OperationalWidgets.App where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Functor (void, (<$>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Unit (Unit)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.State (computeInitialState)
import Functions.JSState (modifyAppState)
import OperationalWidgets.HomePageWidget (HomePageExitStatus(..), homePageWidget)
import Views.LoginFormView (emptyForm)
import Views.LandingPageView (landingPageView, LandingPageView(..))

app :: Widget HTML Unit
app = app' Nothing

  where 
    app' :: Maybe String -> Widget HTML Unit
    app' maybeUsername = do
      initialState <- liftEffect computeInitialState
      liftAff $ modifyAppState initialState
      res <- do
        -- let form = fromMaybe emptyForm ((\u -> { username: u, password: "" }) <$> maybeUsername )
        -- landingPageView (LoginView Default form)
        
        -- !!! AUTOLOGIN FOR DEVELOPING !!! --
        let form = fromMaybe {username: "joe", password: "clipperz"} ((\u -> { username: u, password: "" }) <$> maybeUsername )
        landingPageView (LoginView Loading form)

        -- -------------------------------- --

        homePageWidget
      case res of
        Clean -> app' Nothing
        ReadyForLogin username -> app' (Just username)
