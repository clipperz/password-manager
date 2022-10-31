module OperationalWidgets.DeleteUserWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, div, div')
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (either)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError)
import DataModel.Index (Index(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.User (deleteUser)
import Views.SimpleWebComponents (loadingDiv, simpleButton, confirmationWidget, simpleButton)

data DeleteUserWidgetAction = PleaseDelete | DoNothing | FailedDelete AppError | Done

deleteUserWidget :: Index -> WidgetState -> Widget HTML Unit
deleteUserWidget index state = do
  res <- case state of
    Default -> div' [simpleButton "Delete account" false PleaseDelete]
    Loading -> do
      let deleteUserOp = liftAff $ (either FailedDelete (\_ -> Done) <$> (runExceptT $ deleteUser index))
      div' [loadingDiv, simpleButton "Delete account" true DoNothing] <|> deleteUserOp
    Error err -> div' [text err, simpleButton "Delete account" false PleaseDelete]
  case res of
    PleaseDelete -> do
      confirmation <- div [] [confirmationWidget "Are you sure you want to delete your account? You won't be able to recover it."]
      if confirmation then deleteUserWidget index Loading else deleteUserWidget index Default
    DoNothing -> deleteUserWidget index Default
    FailedDelete err -> deleteUserWidget index (Error (show err))
    Done -> pure unit
