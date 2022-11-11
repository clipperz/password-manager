module OperationalWidgets.App where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, p, div)
import Concur.React.Props as Props
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semigroup ((<>))
import Data.Unit (Unit)
import DataModel.Proxy (Proxy(..))
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
      initialState' <- liftEffect $ runExceptT computeInitialState
      case initialState' of
        Right initialState@{proxy} -> do
          liftAff $ modifyAppState initialState
          let offlineCopyBanner = case proxy of
                                    OfflineProxy _ -> [p [Props.className "notice"] [text "Offline copy"]]
                                    _ -> []
          res <- div [Props.className "wrapper"] $ offlineCopyBanner <> [
            do
            let form = fromMaybe emptyForm ((\u -> { username: u, password: "" }) <$> maybeUsername )
            landingPageView (LoginView Default form)
            -- !!! AUTOLOGIN FOR DEVELOPING !!! --
            -- let form = fromMaybe {username: "joe", password: "clipperz"} ((\u -> { username: u, password: "" }) <$> maybeUsername )
            -- landingPageView (LoginView Loading form)
            -- -------------------------------- --
            homePageWidget
          ]
          case res of
            Clean -> app' Nothing
            ReadyForLogin username -> app' (Just username)

        Left _ -> text "Could not initialize app"
