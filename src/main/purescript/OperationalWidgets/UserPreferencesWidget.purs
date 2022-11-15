module OperationalWidgets.UserPreferencesWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, hold, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, text, h3, h1)
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor (void, (<$>))
import Data.Maybe (Maybe(..))
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError)
import DataModel.User (UserPreferences)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Users (updateUserPreferences)
import Functions.JSState (getAppState)
import Views.PasswordGenerator (settingsWidget)
import Views.SimpleWebComponents (simpleButton, loadingDiv)

data PreferencesWidgetAction = NewSettings UserPreferences | SettingsChanged (Either AppError Unit)

userPreferencesWidget :: WidgetState -> Widget HTML Unit
userPreferencesWidget wstate = do
  eitherUP <- liftEffect $ getAppState
  case eitherUP of
    Left err -> void $ div [] [text (prettyShow err)] 
    Right { userPreferences: Nothing } -> void $ div [] [text "Could not load user preferences at login"] 
    Right { userPreferences: Just up } -> do
      go wstate up
      pure unit

  where
    go state up = do
      res <- case state of
        Default -> NewSettings <$> (div [] [userPreferencesView up])
        Loading -> (NewSettings <$> (div [] [loadingDiv, userPreferencesView up])) <|> (SettingsChanged <$> (liftAff $ runExceptT $ updateUserPreferences up))
        Error err -> NewSettings <$> (div [] [userPreferencesView up, errorDiv err])
      case res of
        NewSettings up' -> go Loading up'
        SettingsChanged (Right _) -> userPreferencesWidget Default
        SettingsChanged (Left err) -> go (Error (show err)) up

    errorDiv err = div [] [text err]

    userPreferencesView :: UserPreferences -> Widget HTML UserPreferences
    userPreferencesView up@{ passwordGeneratorSettings } = div [] [
      demand $ do
        _ <- hold unit $ void $ h1 [] [text "Preferences"]
        _ <- hold unit $ void $ h3 [] [text "Password generator"]
        pswdSettings <- loopW passwordGeneratorSettings settingsWidget
        let newUP = up { passwordGeneratorSettings = pswdSettings }
        fireOnce (simpleButton "Change preferences" (pswdSettings == passwordGeneratorSettings) newUP)
    ]
