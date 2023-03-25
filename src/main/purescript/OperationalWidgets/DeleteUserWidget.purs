module OperationalWidgets.DeleteUserWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, loopS, loopW, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (text, div, h1, div', form, p)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (runExceptT, except, mapExceptT, ExceptT(..))
import Control.Semigroupoid ((<<<))
import Data.Either (either, Either(..))
import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>), (<$), void)
import Data.HeytingAlgebra ((||), (&&), not)
import Data.List (List(..), length)
import Data.Maybe (Maybe(..))
import Data.Operation (runOperation)
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..))
import DataModel.Index (Index(..))
import DataModel.WidgetState (WidgetState(..))
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Users (getIndex)
import Functions.JSState (getAppState)
import Functions.Pin (deleteCredentials)
import Functions.User (deleteUser, deleteUserSteps)
import Record (merge)
import Views.Components (ClassName(..), verySimpleInputWidget, InputType(..), Enabled(..), Placeholder(..), Label(..))
import Views.LoginFormView (emptyForm)
import Views.SimpleWebComponents (loadingDiv, loadingBar, simpleButton, confirmationWidget, simpleUserSignal, simpleCheckboxSignal, simplePasswordInputWidget)
import Web.HTML (window)
import Web.HTML.Window (localStorage)

data DeleteUserWidgetAction = PleaseDelete DeleteFormValue | DoNothing | FailedDelete AppError | Done

type Credentials = {username:: Maybe String, password:: Maybe String }

type DeleteFormValue = {username :: String, password :: String, notRecoverable :: Boolean}
emptyFormValues = {username: "", password: "", notRecoverable: false}

emptyCredentials :: Credentials
emptyCredentials = {username: Nothing, password: Nothing}

deleteUserWidget :: Widget HTML Unit
deleteUserWidget = do
  credentials :: Credentials <- liftEffect $ either (\_ -> {username: Nothing, password: Nothing}) (\s -> {username: s.username, password: s.password}) <$> getAppState
  newIndex <- ((Right (Index Nil)) <$ (deleteForm emptyCredentials emptyFormValues (Enabled false))) <|> (liftAff $ runExceptT $ getIndex)
  case newIndex of
    Right index -> go index credentials Default
    Left err -> div' [text (show err), void (deleteForm emptyCredentials emptyFormValues (Enabled false))] 

  where
    go :: Index -> Credentials -> WidgetState -> Widget HTML Unit
    go index@(Index entries) credentials state = do
      res <- case state of
        Default -> deleteForm credentials emptyFormValues (Enabled true)
        Loading -> do
          let total = length entries
          res <- runOperation (Right unit) $ deleteUserSteps index (\i -> div [Props._id "deleteUserArea"] [form [] [
            h1 [] [text "Delete account"]
          , div [Props.className "loadingBarContainer"] [
              div[Props.className "loadingBar", Props.style {width: "" <> show ((i + 1) * 100 / total) <> "%"}] []  
            ]
          , p [] [text ("Deleting " <> show (i + 1) <> " of " <> show total)]
          ]]) text
          pure $ case res of
            Right (Left err) -> FailedDelete err
            Left (Left err) -> FailedDelete err
            Right _ -> FailedDelete $ InvalidOperationError "Operation was not completed, but no other steps were programmed."
            Left (Right _) -> Done
        Error err -> div' [text err, deleteForm credentials emptyFormValues (Enabled true)] 
      case res of
        PleaseDelete f -> do
          confirmation <- (false <$ deleteForm credentials f (Enabled false)) <|> confirmationWidget "Are you sure you want to delete your account? You won't be able to recover it."
          if confirmation then go index credentials Loading else go index credentials Default
        DoNothing -> go index credentials Default
        FailedDelete err -> go index credentials (Error (show err))
        Done -> pure unit

    deleteForm :: Credentials -> DeleteFormValue -> Enabled -> Widget HTML DeleteUserWidgetAction
    deleteForm credentials formValues (Enabled enabled) = div [Props._id "deleteUserArea"] [form [Props.disabled (not enabled)] [
      h1 [] [text "Delete account"]
    , demand $ do
        formValues' <- loopS formValues $ \{username, password, notRecoverable} -> do
          username' :: String  <- loopW username $ verySimpleInputWidget (InputType "text")     (ClassName "username") (Label "Username")   (Enabled true) (Placeholder "username")   (matchingValueClassName credentials.username)
          password' :: String  <- loopW password $ verySimpleInputWidget (InputType "password") (ClassName "password") (Label "Passphrase") (Enabled true) (Placeholder "passphrase") (matchingValueClassName credentials.password)
          checkbox' :: Boolean <- simpleCheckboxSignal "warning" (text "All my data will be permanently deleted. I understand that this action cannot be undone or canceled.") notRecoverable
          pure { username: username', password: password', notRecoverable: checkbox' }
        fireOnce (simpleButton "delete" "Delete account" (not ((Just formValues'.username) == credentials.username && (Just formValues'.password) == credentials.password && formValues'.notRecoverable)) (PleaseDelete formValues'))
    ]]

    matchingValueClassName :: Maybe String -> String -> Maybe ClassName
    matchingValueClassName expectedValue value = expectedValue >>= (\expectedValue' -> if expectedValue' == value then (Just $ ClassName "valid") else Nothing)
