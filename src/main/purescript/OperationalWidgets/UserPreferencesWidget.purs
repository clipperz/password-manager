module OperationalWidgets.UserPreferencesWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Unit (Unit, unit)
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.WidgetState (WidgetState(..))

userPreferencesWidget :: WidgetState -> Widget HTML Unit
userPreferencesWidget state = do
  res <- case state of
    Default -> div [] [userPreferencesView]
    Loading -> div [] [userPreferencesView]
    Error err -> div [] [userPreferencesView, errorDiv err]
  pure unit
  
  where 
    errorDiv err = div [] [text err]
    -- userPreferencesView :: Settings -> Widget HTML Settings
    userPreferencesView = do
      _ <- text "Preferences"
      pure unit
