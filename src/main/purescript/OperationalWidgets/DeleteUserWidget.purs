module OperationalWidgets.DeleteUserWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, loopS, loopW, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (text, div, h1, div', form, p)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (either, Either(..))
import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>), (<$), void)
import Data.HeytingAlgebra (not, (&&))
import Data.List (List(..), length)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Operation (runOperation)
import Data.Semigroup ((<>))
import Data.Semiring ((+), (*))
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..))
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.Index (Index(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Users (getIndexWithState)
import Functions.JSState (getAppState)
import Functions.User (deleteUserSteps)
import Views.Components (ClassName(..), verySimpleInputWidget, InputType(..), Enabled(..), Placeholder(..), Label(..))
import Views.SimpleWebComponents (confirmationWidget, simpleButton, simpleCheckboxSignal)

data DeleteUserWidgetAction = PleaseDelete DeleteFormValue | DoNothing | FailedDelete AppError | Done

type DeleteFormValue = {username :: String, password :: String, notRecoverable :: Boolean}
emptyFormValues :: { notRecoverable :: Boolean
                   , password :: String
                   , username :: String
                   }
emptyFormValues = {username: "", password: "", notRecoverable: false}

deleteUserWidget :: Widget HTML Unit
deleteUserWidget = do
  credentials :: Credentials <- liftEffect $ either (\_ -> emptyCredentials) (\s -> {username: fromMaybe "" s.username, password: fromMaybe "" s.password}) <$> getAppState
  newIndex <- ((Right (Index Nil)) <$ (deleteForm emptyCredentials emptyFormValues (Enabled false))) <|> (liftAff $ runExceptT $ getIndexWithState)
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
        fireOnce (simpleButton "delete" "Delete account" (not (formValues'.username == credentials.username && formValues'.password == credentials.password && formValues'.notRecoverable)) (PleaseDelete formValues'))
    ]]

    matchingValueClassName :: String -> String -> Maybe ClassName
    matchingValueClassName expectedValue value = if expectedValue == value then (Just $ ClassName "valid") else Nothing
