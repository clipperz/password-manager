module OperationalWidgets.DeleteUserWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, loopS, loopW, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (text, div, h1, div')
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (runExceptT, except, mapExceptT, ExceptT(..))
import Control.Semigroupoid ((<<<))
import Data.Either (either, Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra ((||), (&&), not)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError)
import DataModel.Index (Index)
import DataModel.WidgetState (WidgetState(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.JSState (getAppState)
import Functions.Pin (deleteCredentials)
import Functions.User (deleteUser)
import Record (merge)
import Views.LoginFormView (emptyForm)
import Views.SimpleWebComponents (loadingDiv, simpleButton, confirmationWidget, simpleUserSignal, simpleCheckboxSignal, simplePasswordInputWidget)
import Web.HTML (window)
import Web.HTML.Window (localStorage)

data DeleteUserWidgetAction = PleaseDelete | DoNothing | FailedDelete AppError | Done

deleteUserWidget :: Index -> WidgetState -> Widget HTML Unit
deleteUserWidget index state = do
  res <- case state of
    Default -> deleteForm emptyForm
    Loading -> do
      let deleteUserAndPin = do
                              deleteUser index
                              (ExceptT $ Right <$> (liftEffect (window >>= localStorage))) >>= (\v -> mapExceptT liftEffect (deleteCredentials v)) 
      let deleteUserOp = liftAff $ (either FailedDelete (\_ -> Done) <$> (runExceptT $ deleteUserAndPin))
      div' [loadingDiv, simpleButton "Delete account" true DoNothing] <|> deleteUserOp
    Error err -> div' [text err, simpleButton "Delete account" false PleaseDelete]
  case res of
    PleaseDelete -> do
      confirmation <- div [] [confirmationWidget "Are you sure you want to delete your account? You won't be able to recover it."]
      if confirmation then deleteUserWidget index Loading else deleteUserWidget index Default
    DoNothing -> deleteUserWidget index Default
    FailedDelete err -> deleteUserWidget index (Error (show err))
    Done -> pure unit

  where
    deleteForm creds@{ username, password } = div [] [
      h1 [] [text "Delete account"]
    , demand $ do
        formValues <- loopS (merge creds {notRecoverable: false}) $ \{username, password, notRecoverable} -> do
          username' :: String <- simpleUserSignal username
          password' :: String <- loopW password (simplePasswordInputWidget "password" (text "Password"))
          checkbox' :: Boolean <- simpleCheckboxSignal "not_recoverable" (text "All my data will be permanently deleted. I understand that this action cannot be undone or canceled.") true notRecoverable
          pure { username: username', password: password', notRecoverable: checkbox' }
        fireOnce (submitWidget formValues)
    ] 

    submitWidget :: { username :: String, password :: String, notRecoverable :: Boolean } -> Widget HTML DeleteUserWidgetAction
    submitWidget f@{ username, password, notRecoverable } = do
      check <- liftEffect $ if username == "" || password == "" then pure (Right false) else checkC f
      case check of
        Left err -> do
          log $ show err
          simpleButton "Delete account" true PleaseDelete
        Right b -> do
          let enable = b && notRecoverable
          simpleButton "Delete account" (not enable) PleaseDelete
    
    checkC :: { username :: String, password :: String, notRecoverable :: Boolean } -> Effect (Either AppError Boolean)
    checkC { username, password } = runExceptT $ do
      { username: u, password: p } <- ExceptT $ liftEffect $ getAppState
      except $ Right $ u == (Just username) && p == (Just password)
