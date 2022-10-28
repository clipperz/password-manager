module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (text, div, div', fieldset)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HexString (HexString, fromArrayBuffer)
import Data.HeytingAlgebra ((&&), not)
import Data.List (List(..), (:), concat, fromFoldable)
import Data.Maybe (Maybe(..), isJust)
import Data.Show (show)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Index (Index(..), CardEntry)
import DataModel.SRP as SRP
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (postCard, updateIndex)
import Functions.Import (decodeImport, parseHTMLImport, decodeHTML)
import Functions.JSState (getAppState)
import Functions.Password (standardPasswordStrengthFunction)
import Functions.SRP as SRP
import Views.SimpleWebComponents (loadingDiv, simpleFileInputWidget, simpleButton, simpleUserSignal, simplePasswordSignal, simpleCheckboxSignal, simpleVerifiedPasswordSignal, PasswordForm)
import OperationalWidgets.ImportWidget (importWidget)
import Record (merge)

data UserAreaAction = Loaded (Either AppError Index) | Logout

userAreaWidget :: Index -> Widget HTML UserAreaAction
userAreaWidget index@(Index entries) = 
  div [Props._id "userSidebar"] [
    Loaded <$> importWidget index
  , changePasswordWidget Default emptyChangePasswordDataForm
  , simpleButton "Logout" false Logout
  ]

type ChangePasswordDataForm = { username       :: String
                              , oldPassword    :: String
                              , password       :: String
                              , verifyPassword :: String
                              , notRecoverable :: Boolean
                              }

emptyChangePasswordDataForm = { username: "", oldPassword: "", password: "", verifyPassword: "", notRecoverable: false}

data ChangePasswordWidgetAction = Change ChangePasswordDataForm | DoNothing | Done

changePasswordWidget :: SRP.SRPConf -> WidgetState -> ChangePasswordDataForm -> forall a. Widget HTML a
changePasswordWidget conf state changeForm = do
  res <- case state of
    Default   -> div' [Change <$> form false]
    Loading   -> div' [loadingDiv, DoNothing <$ form true] -- TODO:
    Error err -> div' [errorDiv err, Change <$> form false]
  case res of
    DoNothing -> changePasswordWidget conf state changeForm
    Change form -> changePasswordWidget conf Loading form
    Done -> div' [] [changePasswordWidget conf Default emptyChangePasswordDataForm, text "Password changed"]

  where 
    errorDiv err = div' [text err ]
    form :: Boolean -> Widget HTML ChangePasswordDataForm
    form disabled = fieldset [(Props.disabled disabled)] [
      do
        signalResult <- demand $ do
          formValues :: ChangePasswordDataForm <- loopS changeForm $ \{username, oldPassword, password, verifyPassword, checkbox} -> do
            username' :: String <- simpleUserSignal username
            oldPassword' :: String <- simplePasswordSignal oldPassword
            eitherPassword :: Either PasswordForm String <- simpleVerifiedPasswordSignal standardPasswordStrengthFunction $ Left {password, verifyPassword}
            checkbox' :: Boolean <- simpleCheckboxSignal "not_recoverable" (text "I understand Clipperz won't be able to recover a lost password") checkbox
            case eitherPassword of
              Left  passwords -> pure $ merge passwords { username: username', oldPassword: oldPassword', checkbox: checkbox'}
              Right s         -> pure { username: username', oldPassword: oldPassword', password: s, verifyPassword: s, checkbox: checkbox' }
          result :: Maybe HexString <- fireOnce (submitWidget formValues)
          pure result
        -- liftEffect $ log $ "signalResult " <> show signalResult
        pure signalResult
    ]

    submitWidget :: ChangePasswordDataForm -> Widget HTML HexString
    submitWidget f@{ username, password } = do
      newC <- fromArrayBuffer <$> (SRP.prepareC conf username password)
      eitherAppstate <- liftEffect $ getAppState
      case eitherAppstate of
        Left err -> do
          log $ show err
          simpleButton "Change password" true newC
        Right state@{c: mc} -> do
          let enable = isJust $ (\b -> b && (isNewDataValid f)) <$> ((\c -> c == newC) <$> mc)
          simpleButton "Change password" (not enable) newC

    isNewDataValid :: ChangePasswordDataForm -> Boolean
    isNewDataValid f@{password, verifyPassword, notRecoverable} = password == verifyPassword && notRecoverable
