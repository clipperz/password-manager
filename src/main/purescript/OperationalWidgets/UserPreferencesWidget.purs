module OperationalWidgets.UserPreferencesWidget
  ( userPreferencesWidget
  )
  where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, hold, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, text, h3, h1)
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, except, ExceptT)
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor (void, (<$>))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.User (UserPreferences(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Users (updateUserPreferences, getUserPreferences)
import Functions.JSState (getAppState)
import Functions.Timer (activateTimer, stopTimer)
import Views.PasswordGenerator (settingsWidget)
import Views.SimpleWebComponents (simpleButton, loadingDiv, simpleCheckboxWidget, simpleInputWidget)

data PreferencesWidgetAction = NewSettings UserPreferences | SettingsChanged (Either AppError Unit)

userPreferencesWidget :: WidgetState -> Widget HTML Unit
userPreferencesWidget wstate = do
  eitherUP <- liftAff $ runExceptT getUserPreferences           
  case eitherUP of
    Left err -> void $ div [] [text (prettyShow err)] 
    Right up -> do
      go wstate up
      pure unit

  where
    go state up@(UserPreferences r) = do
      res <- case state of
        Default -> NewSettings <$> (div [] [userPreferencesView up])
        Loading -> do
          let updateOp = do
                          liftEffect $ stopTimer
                          case r.automaticLock of
                            Nothing -> pure unit
                            Just n -> liftEffect $ activateTimer n
                          liftAff $ runExceptT $ updateUserPreferences up
          (NewSettings <$> (div [] [loadingDiv, userPreferencesView up])) <|> (SettingsChanged <$> updateOp)
        Error err -> NewSettings <$> (div [] [userPreferencesView up, errorDiv err])
      case res of
        NewSettings up' -> go Loading up'
        SettingsChanged (Right _) -> userPreferencesWidget Default
        SettingsChanged (Left err) -> go (Error (show err)) up

    errorDiv err = div [] [text err]

    userPreferencesView :: UserPreferences -> Widget HTML UserPreferences
    userPreferencesView up@(UserPreferences r@{ passwordGeneratorSettings, automaticLock }) = div [] [
      demand $ do
        _ <- hold unit $ void $ h1 [] [text "Preferences"]
        _ <- hold unit $ void $ h3 [] [text "Lock"]
        lockSettings <- loopW automaticLock automaticLockWidget
        _ <- hold unit $ void $ h3 [] [text "Password generator"]
        pswdSettings <- loopW passwordGeneratorSettings settingsWidget
        let newUP = UserPreferences $ r { passwordGeneratorSettings = pswdSettings, automaticLock = lockSettings }
        fireOnce (simpleButton "Change preferences" (up == newUP) newUP)
    ]

    automaticLockWidget :: Maybe Int -> Widget HTML (Maybe Int)
    automaticLockWidget lockTime = do
      res <- div [] [
        ChangeEnable <$> simpleCheckboxWidget "lockEnabled" (text "Enable auto-lock") false (isJust lockTime)
      , ChangeValue <$> simpleInputWidget "lockTime" (text "Lock timeout:") (isNothing lockTime) "Lock time" (show (fromMaybe 0 lockTime)) "number"
      ]
      case res of
        ChangeEnable false -> pure Nothing
        ChangeEnable true -> pure (Just 10)
        ChangeValue v -> 
          case fromString v of
            Just newTime -> pure $ Just newTime
            Nothing -> pure (Just 0)

data AutomaticLockWidgetAction = ChangeEnable Boolean | ChangeValue String
