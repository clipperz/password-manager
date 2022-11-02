module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div)
import Concur.React.Props as Props
import Data.Either (Either)
import Data.Functor ((<$>), (<$))
import DataModel.AppState (AppError)
import DataModel.Index (Index)
import DataModel.WidgetState (WidgetState(..))
import Views.SimpleWebComponents (simpleButton)
import OperationalWidgets.ImportWidget (importWidget)
import OperationalWidgets.ChangePasswordWidget (changePasswordWidget, emptyChangePasswordDataForm)
import OperationalWidgets.DeleteUserWidget (deleteUserWidget)

data UserAreaAction = Loaded (Either AppError Index) | Logout | DeleteAccount

userAreaWidget :: Index -> Widget HTML UserAreaAction
userAreaWidget index = 
  div [Props._id "userSidebar"] [
    Loaded <$> importWidget index
  , changePasswordWidget Default emptyChangePasswordDataForm
  , DeleteAccount <$ deleteUserWidget index Default
  , simpleButton "Logout" false Logout
  ]
