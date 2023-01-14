module OperationalWidgets.UserPreferencesWidget
  ( userPreferencesWidget
  )
  where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, hold, loopW, loopS)
import Concur.React (HTML)
import Concur.React.DOM (div, text, form, h3, h1, ol_, li_, label, span, input)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, except, ExceptT)
import Data.Either (Either(..), note, isRight, either, hush)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor (void, (<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust, isNothing, fromMaybe)
import Data.PrettyShow (prettyShow)
import Data.Show (show)
import Data.Tuple (Tuple(..))
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
                            Left  _ -> pure unit
                            Right n -> liftEffect $ activateTimer n
                          liftAff $ runExceptT $ updateUserPreferences up
          (NewSettings <$> (div [] [loadingDiv, userPreferencesView up])) <|> (SettingsChanged <$> updateOp)
        Error err -> NewSettings <$> (div [] [userPreferencesView up, errorDiv err])
      case res of
        NewSettings up' -> go Loading up'
        SettingsChanged (Right _) -> userPreferencesWidget Default
        SettingsChanged (Left err) -> go (Error (show err)) up

    errorDiv err = div [] [text err]

    userPreferencesView :: UserPreferences -> Widget HTML UserPreferences
    userPreferencesView up = form [] [
      demand $ do
        newUP <- loopS up (\(UserPreferences r@{ passwordGeneratorSettings, automaticLock }) -> do
          _ <- hold unit $ void $ h1 [] [text "Preferences"]
          Tuple lockSettings pswdSettings <- ol_ [] do
            lockSettings' <- li_ [] do
              _ <- hold unit $ void $ h3 [] [text "Lock"]
              loopW automaticLock automaticLockWidget
            pswdSettings' <- li_ [] do
              _ <- hold unit $ void $ h3 [] [text "Password generator"]
              loopW passwordGeneratorSettings settingsWidget
            pure $ Tuple lockSettings' pswdSettings'
          pure $ UserPreferences $ r { passwordGeneratorSettings = pswdSettings, automaticLock = lockSettings }
        )
        fireOnce (simpleButton "Save Preferences" (up == newUP) newUP)
    ]

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
                span [Props.className "label"] [text "Lock timeout"]
              , input [
                  Props._type "number"
                , Props.placeholder "Lock time"
                , Props.value $ show value
                , Props.disabled (not isEnabled)
                , Props.onChange
                ]
              , span [Props.className "units"] [text "minutes"]
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
