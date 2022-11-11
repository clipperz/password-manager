module OperationalWidgets.ChangePasswordWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (text, div, div', h1, fieldset)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Data.Either (Either(..), either)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra ((&&), (||), not)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import DataModel.AppState (AppError)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.JSState (getAppState)
import Functions.Password (standardPasswordStrengthFunction)
import Functions.User (changeUserPassword)
import Views.SimpleWebComponents (PasswordForm, loadingDiv, simpleButton, simpleCheckboxSignal, simplePasswordInputWidget, simpleUserSignal, simpleVerifiedPasswordSignal)
import Record (merge)

type ChangePasswordDataForm = { username       :: String
                              , oldPassword    :: String
                              , password       :: String
                              , verifyPassword :: String
                              , notRecoverable :: Boolean
                              }
emptyChangePasswordDataForm :: { 
    notRecoverable :: Boolean
  , oldPassword :: String
  , password :: String
  , username :: String
  , verifyPassword :: String
  }
emptyChangePasswordDataForm = { username: "", oldPassword: "", password: "", verifyPassword: "", notRecoverable: false}

data ChangePasswordWidgetAction = Change ChangePasswordDataForm | ChangeFailed AppError | DoNothing | Done

changePasswordWidget :: WidgetState -> ChangePasswordDataForm -> forall a. Widget HTML a
changePasswordWidget state changeForm = go state changeForm 

  where 
    go :: WidgetState -> ChangePasswordDataForm -> forall a. Widget HTML a
    go s cf@{ username, password } = do
      res <- case s of
        Default   -> div [Props._id "changePasswordArea"] [Change <$> form false]
        Loading   -> do
          let changePasswordOp = liftAff $ (either ChangeFailed (\_ -> Done) <$> (runExceptT $ changeUserPassword username password))
          div [Props._id "changePasswordArea"] [loadingDiv, DoNothing <$ form true] <|> changePasswordOp
        Error err -> div [Props._id "changePasswordArea"] [errorDiv err, Change <$> form false]
      case res of
        DoNothing -> go s cf
        Change formData -> go Loading formData
        ChangeFailed err -> go (Error (show err)) cf
        Done -> div' [go Default emptyChangePasswordDataForm, text "Password changed"]

    errorDiv err = div' [text err ]

    form :: Boolean -> Widget HTML ChangePasswordDataForm
    form disabled = fieldset [(Props.disabled disabled)] [
      h1 [] [text "Change passphrase"]
    , do
        signalResult <- demand $ do
          formValues :: ChangePasswordDataForm <- loopS changeForm $ \{username, oldPassword, password, verifyPassword, notRecoverable} -> do
            username' :: String <- simpleUserSignal username
            oldPassword' :: String <- loopW oldPassword (simplePasswordInputWidget "password" (text "Old password"))
            eitherPassword :: Either PasswordForm String <- simpleVerifiedPasswordSignal standardPasswordStrengthFunction $ Left {password, verifyPassword}
            checkbox' :: Boolean <- simpleCheckboxSignal "not_recoverable" (text "I understand Clipperz won't be able to recover a lost password") false notRecoverable
            case eitherPassword of
              Left  passwords -> pure $ merge passwords { username: username', oldPassword: oldPassword', notRecoverable: checkbox'}
              Right s         -> pure { username: username', oldPassword: oldPassword', password: s, verifyPassword: s, notRecoverable: checkbox' }
          result :: Maybe ChangePasswordDataForm <- fireOnce (submitWidget formValues)
          pure result
        pure signalResult
    ]

    submitWidget :: ChangePasswordDataForm -> Widget HTML ChangePasswordDataForm
    submitWidget f@{ username, oldPassword } = do
      check <- liftEffect $ if username == "" || oldPassword == "" then pure (Right false) else checkC f
      case check of
        Left err -> do
          log $ show err
          simpleButton "Change password" true f
        Right b -> do
          let enable = b && (isNewDataValid f)
          simpleButton "Change password" (not enable) f
    
    checkC :: ChangePasswordDataForm -> Effect (Either AppError Boolean)
    checkC { username, oldPassword } = runExceptT $ do
      { username: u, password } <- ExceptT $ liftEffect $ getAppState
      except $ Right $ u == (Just username) && password == (Just oldPassword)

    isNewDataValid :: ChangePasswordDataForm -> Boolean
    isNewDataValid {password, verifyPassword, notRecoverable} = password == verifyPassword && notRecoverable
