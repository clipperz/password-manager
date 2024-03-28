module Views.UserPreferencesView
  ( userPreferencesView
  )
  where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, hold, loopW, loopS)
import Concur.React (HTML)
import Concur.React.DOM (div, text, form, h3, h1, ol_, li_, label, span, input)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Either (Either(..), either, hush, isRight)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor (void, (<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.UserVersions.User (UserPreferences(..))
import Views.PasswordGenerator (settingsWidget)
import Views.SimpleWebComponents (simpleButton)

userPreferencesView :: UserPreferences -> Widget HTML UserPreferences
userPreferencesView userPreferences = div [Props._id "userPreferencesArea"] [ form [] [
  demand $ do
    newUserPreferences <- loopS userPreferences (\(UserPreferences { passwordGeneratorSettings, automaticLock }) -> do
      _ <- hold unit $ void $ h1 [] [text "Preferences"]
      Tuple lockSettings pswdSettings <- ol_ [] do
        lockSettings' <- li_ [] do
          _ <- hold unit $ void $ h3 [] [text "Lock"]
          loopW automaticLock automaticLockWidget
        pswdSettings' <- li_ [] do
          _ <- hold unit $ void $ h3 [] [text "Password generator"]
          loopW passwordGeneratorSettings settingsWidget
        pure $ Tuple lockSettings' pswdSettings'
      pure $ UserPreferences { passwordGeneratorSettings: pswdSettings, automaticLock: lockSettings }
    )
    fireOnce (simpleButton "save" "Save Preferences" (userPreferences == newUserPreferences) newUserPreferences)
]]

  where
    automaticLockWidget :: Either Int Int -> Widget HTML (Either Int Int)
    automaticLockWidget lockTime = do
      let isEnabled = (isRight lockTime) :: Boolean
      let value = either (\l -> l) (\r -> r) lockTime
      res <- div [] [
        ChangeEnable  <$> label [Props.className "autolock"] [
                            span [] [text "Enable auto-lock"]
                          , (not isEnabled) <$ input [Props._type "checkbox", Props.checked isEnabled, Props.onChange]
                          ]
      , ChangeValue   <$> (Props.unsafeTargetValue) <$> label [Props.classList [Just "lockTime", (\_ -> "enabled") <$> hush lockTime]] [
                span  [Props.className "label"] [text "Lock timeout"]
              , input [Props._type "number", Props.placeholder "Lock time", Props.value $ show value, Props.disabled (not isEnabled), Props.onChange]
              , span  [Props.className "units"] [text "minutes"]
              ]
      ]
      case res of
        ChangeEnable  false -> pure (Left  value)
        ChangeEnable  true  -> pure (Right value)
        ChangeValue   v     -> 
          case fromString v of
            Just newTime  -> pure (Right newTime)
            Nothing       -> pure (Right value)

data AutomaticLockWidgetAction = ChangeEnable Boolean | ChangeValue String
