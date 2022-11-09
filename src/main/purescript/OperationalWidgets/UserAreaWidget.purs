module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Either (Either)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import DataModel.AppState (AppError)
import DataModel.Index (Index)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Logout (doLogout)
import Views.SimpleWebComponents (simpleButton)
import OperationalWidgets.ImportWidget (importWidget)
import OperationalWidgets.ExportWidget (exportWidget)
import OperationalWidgets.ChangePasswordWidget (changePasswordWidget, emptyChangePasswordDataForm)
import OperationalWidgets.DeleteUserWidget (deleteUserWidget)
import OperationalWidgets.PinWidget (setPinWidget)

data UserAreaAction = Loaded (Either AppError Index) | Lock | Logout | DeleteAccount | NoAction

userAreaWidget :: Index -> Boolean -> Widget HTML UserAreaAction
userAreaWidget index isOffline = do
  res <- if isOffline then div [Props._id "userSidebar"] [
      simpleButton "Close user area" false NoAction
    , NoAction <$ exportWidget index
    , setPinWidget Default
    , simpleButton "Lock" false Lock
    , simpleButton "Logout" false Logout
    ]
    else div [Props._id "userSidebar"] [
        simpleButton "Close user area" false NoAction
      , Loaded <$> importWidget index
      , NoAction <$ exportWidget index
      , setPinWidget Default
      , changePasswordWidget Default emptyChangePasswordDataForm
      , DeleteAccount <$ deleteUserWidget index Default
      , simpleButton "Lock" false Lock
      , simpleButton "Logout" false Logout
      ]
  case res of
    Lock -> do
      (simpleButton "Lock" true Lock) <|> (Lock <$ (liftAff $ doLogout true))
    Logout -> do
      (simpleButton "Logout" true Logout) <|> (Logout <$ (liftAff $ doLogout false))
    _ -> pure res
