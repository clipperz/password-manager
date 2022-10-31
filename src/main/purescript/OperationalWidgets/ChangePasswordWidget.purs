module OperationalWidgets.ChangePasswordWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand, loopW, hold)
import Concur.React (HTML)
import Concur.React.DOM (text, div, div', fieldset)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Data.Either (Either(..), either)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HexString (HexString, hex, fromArrayBuffer)
import Data.HeytingAlgebra ((&&), (||), not)
import Data.List (List(..), (:), concat, fromFoldable)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.PrettyShow (prettyShow)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Credentials (Credentials)
import DataModel.Index (Index(..), CardEntry)
import DataModel.SRP as SRP
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard)
import Functions.Communication.Users (updateIndex)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Functions.JSState (getAppState)
import Functions.Password (standardPasswordStrengthFunction)
import Functions.SRP as SRP
import Functions.State (getSRPConf)
import Functions.User (changeUserPassword)
import Views.SimpleWebComponents (loadingDiv, simplePasswordInputWidget, simpleFileInputWidget, simpleButton, simpleUserSignal, simplePasswordSignal, simpleCheckboxSignal, simpleVerifiedPasswordSignal, PasswordForm)
import OperationalWidgets.ImportWidget (importWidget)
import Record (merge)

type ChangePasswordDataForm = { username       :: String
                              , oldPassword    :: String
                              , password       :: String
                              , verifyPassword :: String
                              , notRecoverable :: Boolean
                              }

emptyChangePasswordDataForm = { username: "", oldPassword: "", password: "", verifyPassword: "", notRecoverable: false}

data ChangePasswordWidgetAction = Change ChangePasswordDataForm | ChangeFailed AppError | DoNothing | Done

changePasswordWidget :: WidgetState -> ChangePasswordDataForm -> forall a. Widget HTML a
changePasswordWidget state changeForm = do
  conf <- liftEffect getSRPConf
  case conf of 
    Left err -> go Nothing (Error (prettyShow err)) changeForm
    Right c -> go (Just c) state changeForm 

  where 
    go :: Maybe SRP.SRPConf -> WidgetState -> ChangePasswordDataForm -> forall a. Widget HTML a
    go conf s cf@{ username, password } = do
      res <- case s of
        Default   -> div [Props._id "changePasswordArea"] [Change <$> form conf false]
        Loading   -> do
          let changePasswordOp = liftAff $ (either ChangeFailed (\_ -> Done) <$> (runExceptT $ changeUserPassword username password))
          div [Props._id "changePasswordArea"] [loadingDiv, DoNothing <$ form conf true] <|> changePasswordOp
        Error err -> div [Props._id "changePasswordArea"] [errorDiv err, Change <$> form conf false]
      case res of
        DoNothing -> go conf s cf
        Change formData -> go conf Loading formData
        ChangeFailed err -> go conf (Error (show err)) cf
        Done -> div' [go conf Default emptyChangePasswordDataForm, text "Password changed"]

    errorDiv err = div' [text err ]

    form :: Maybe SRP.SRPConf -> Boolean -> Widget HTML ChangePasswordDataForm
    form Nothing _ = pure emptyChangePasswordDataForm
    form (Just conf) disabled = fieldset [(Props.disabled disabled)] [
      do
        signalResult <- demand $ do
          formValues :: ChangePasswordDataForm <- loopS changeForm $ \{username, oldPassword, password, verifyPassword, notRecoverable} -> do
            username' :: String <- simpleUserSignal username
            oldPassword' :: String <- loopW oldPassword (simplePasswordInputWidget "password" (text "Old password"))
            eitherPassword :: Either PasswordForm String <- simpleVerifiedPasswordSignal standardPasswordStrengthFunction $ Left {password, verifyPassword}
            checkbox' :: Boolean <- simpleCheckboxSignal "not_recoverable" (text "I understand Clipperz won't be able to recover a lost password") notRecoverable
            case eitherPassword of
              Left  passwords -> pure $ merge passwords { username: username', oldPassword: oldPassword', notRecoverable: checkbox'}
              Right s         -> pure { username: username', oldPassword: oldPassword', password: s, verifyPassword: s, notRecoverable: checkbox' }
          result :: Maybe ChangePasswordDataForm <- fireOnce (submitWidget (Just conf) formValues)
          pure result
        pure signalResult
    ]

    submitWidget :: Maybe SRP.SRPConf -> ChangePasswordDataForm -> Widget HTML ChangePasswordDataForm
    submitWidget conf f@{ username, oldPassword } = do
      check <- liftAff $ if username == "" || oldPassword == "" then pure (Right false) else checkC conf f -- to avoid Aff failures
      case check of
        Left err -> do
          log $ show err
          simpleButton "Change password" true f
        Right b -> do
          let enable = b && (isNewDataValid f)
          simpleButton "Change password" (not enable) f
    
    checkC :: Maybe SRP.SRPConf -> ChangePasswordDataForm -> Aff (Either AppError Boolean)
    checkC Nothing _ = pure $ Right false
    checkC (Just conf) f@{ username, oldPassword } = runExceptT $ do
      appState <- ExceptT $ liftEffect $ getAppState
      oldC <- ExceptT $ Right <$> fromArrayBuffer <$> (SRP.prepareC conf username oldPassword)
      except $ Right $ fromMaybe false $ ((==) oldC) <$> appState.c

    isNewDataValid :: ChangePasswordDataForm -> Boolean
    isNewDataValid f@{password, verifyPassword, notRecoverable} = password == verifyPassword && notRecoverable
