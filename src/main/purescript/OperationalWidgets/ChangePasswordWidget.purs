module OperationalWidgets.ChangePasswordWidget
  ( changePasswordWidget
  , emptyChangePasswordDataForm
  )
  where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, div', form, h1, text)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT(..), except)
import Data.Either (Either(..), either)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra ((&&), (||), not)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import DataModel.AppState (AppError)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect (Effect)
import Effect.Class (liftEffect)
import Functions.JSState (getAppState)
import Functions.User (changeUserPassword)
import Views.SimpleWebComponents (loadingDiv, simpleButton, simpleCheckboxSignal)
import Views.Components (ClassName(..), verySimpleInputWidget, InputType(..), Enabled(..), Placeholder(..), Label(..), entropyMeter)

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

type Credentials = {username:: Maybe String, password:: Maybe String }

changePasswordWidget :: WidgetState -> ChangePasswordDataForm -> forall a. Widget HTML a
changePasswordWidget state changeForm = go state changeForm 

  where 
    go :: WidgetState -> ChangePasswordDataForm -> forall a. Widget HTML a
    go s cf@{ username, password } = do
      currentCredentials :: Credentials <- liftEffect $ either (\_ -> {username: Nothing, password: Nothing}) (\state_ -> {username: state_.username, password: state_.password}) <$> getAppState
      res <- case s of
        Default   -> div [Props._id "changePasswordArea"] [Change <$> formWidget currentCredentials (Enabled true)]
        Loading   -> do
          let changePasswordOp = liftAff $ (either ChangeFailed (\_ -> Done) <$> (runExceptT $ changeUserPassword username password))
          div [Props._id "changePasswordArea"] [loadingDiv, DoNothing <$ formWidget currentCredentials (Enabled false)] <|> changePasswordOp
        Error err -> div [Props._id "changePasswordArea"] [errorDiv err, Change <$> formWidget currentCredentials (Enabled true)]
      case res of
        DoNothing -> go s cf
        Change formData -> go Loading formData
        ChangeFailed err -> go (Error (show err)) cf
        Done -> div' [go Default emptyChangePasswordDataForm, text "Password changed"]

    errorDiv err = div' [text err ]

    formWidget :: Credentials -> Enabled -> Widget HTML ChangePasswordDataForm
    formWidget currentCredentials (Enabled enabled) = form [Props.disabled (not enabled)] [
      h1 [] [text "Change passphrase"]
    , do
        signalResult <- demand $ do
          formValues :: ChangePasswordDataForm <- loopS changeForm $ \{username, oldPassword, password, verifyPassword, notRecoverable} -> do
            username'       :: String   <-  loopW username       $ verySimpleInputWidget (InputType "text")      (ClassName "username")  (Label "Username")        (Enabled true)  (Placeholder "username")              (matchingValueClassName currentCredentials.username)
            oldPassword'    :: String   <-  loopW oldPassword    $ verySimpleInputWidget (InputType "password")  (ClassName "password")  (Label "Old passphrase")    (Enabled true)  (Placeholder "old passphrase")          (matchingValueClassName currentCredentials.password)
            password'       :: String   <-  loopW password       (\p -> (
                                                                    verySimpleInputWidget (InputType "password")  (ClassName "password")  (Label "New passphrase")    (Enabled true)  (Placeholder "new passphrase")          (\_ -> Just $ ClassName "valid") p
                                                                    <>
                                                                    entropyMeter p
                                                                 ))
            verifyPassword' :: String   <-  loopW verifyPassword $ verySimpleInputWidget (InputType "password")  (ClassName "password")  (Label "Verify passphrase") (Enabled true)  (Placeholder "confirm new passphrase")  (matchingValueClassName $ Just password')
            checkbox'       :: Boolean  <-  simpleCheckboxSignal "no_recovery" (text "I understand Clipperz won't be able to recover a lost password") notRecoverable
            pure { username: username', oldPassword: oldPassword', password: password', verifyPassword:verifyPassword', notRecoverable: checkbox' }
          result :: Maybe ChangePasswordDataForm <- fireOnce (submitWidget formValues)
          pure result
        pure signalResult
    ]

    matchingValueClassName :: Maybe String -> String -> Maybe ClassName
    matchingValueClassName expectedValue value = expectedValue >>= (\expectedValue' -> if expectedValue' == value then (Just $ ClassName "valid") else Nothing)

    submitWidget :: ChangePasswordDataForm -> Widget HTML ChangePasswordDataForm
    submitWidget f@{ username, oldPassword } = do
      check     <- liftEffect $ if username == "" || oldPassword == "" then pure (Right false) else checkC f
      disabled  <- pure $ case check of
        Left  _ -> true
        Right b   -> not (b && (isNewDataValid f))
      simpleButton "change_password" "Change passphrase" disabled f
    
    checkC :: ChangePasswordDataForm -> Effect (Either AppError Boolean)
    checkC { username, oldPassword } = runExceptT $ do
      { username: u, password } <- ExceptT $ liftEffect $ getAppState
      except $ Right $ u == (Just username) && password == (Just oldPassword)

    isNewDataValid :: ChangePasswordDataForm -> Boolean
    isNewDataValid {password, verifyPassword, notRecoverable} = password == verifyPassword && notRecoverable
