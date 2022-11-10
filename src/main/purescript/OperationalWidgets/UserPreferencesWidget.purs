module OperationalWidgets.UserPreferencesWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
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
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Users (updateUserPreferences)
import Functions.JSState (getAppState)
import Views.PasswordGenerator (settingsWidget)
import Views.SimpleWebComponents (simpleButton, loadingDiv)

data PreferencesWidgetAction = NewSettings PasswordGeneratorSettings | SettingsChanged (Either AppError Unit)

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
    go state up@{ passwordGeneratorSettings: settings } = do
      res <- case state of
        Default -> NewSettings <$> (div [] [userPreferencesView settings])
        Loading -> (NewSettings <$> (div [] [loadingDiv, userPreferencesView settings])) <|> (SettingsChanged <$> (liftAff $ runExceptT $ updateUserPreferences up))
        Error err -> NewSettings <$> (div [] [userPreferencesView settings, errorDiv err])
      case res of
        NewSettings s -> go Loading (up { passwordGeneratorSettings = s })
        SettingsChanged (Right _) -> userPreferencesWidget Default
        SettingsChanged (Left err) -> go (Error (show err)) up

    errorDiv err = div [] [text err]

    userPreferencesView :: PasswordGeneratorSettings -> Widget HTML PasswordGeneratorSettings
    userPreferencesView up = demand $ do
      values <- loopW up settingsWidget
      fireOnce (simpleButton "Change preferences" (values == up) values)
