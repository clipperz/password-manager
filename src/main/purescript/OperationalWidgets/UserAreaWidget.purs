module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.List (List(..))
import DataModel.AppState (AppError)
import DataModel.Index (Index(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Logout (doLogout)
import Functions.Communication.Users (getIndex)
import Views.SimpleWebComponents (simpleButton)
import OperationalWidgets.ImportWidget (importWidget)
import OperationalWidgets.ExportWidget (exportWidget)
import OperationalWidgets.ChangePasswordWidget (changePasswordWidget, emptyChangePasswordDataForm)
import OperationalWidgets.DeleteUserWidget (deleteUserWidget)
import OperationalWidgets.PinWidget (setPinWidget)

data UserAreaAction = Loaded (Either AppError Index) | Lock | Logout | DeleteAccount | NoAction Index | GetIndexError AppError

userAreaWidget :: Boolean -> Widget HTML UserAreaAction
userAreaWidget isOffline = do
  newIndex <- ((Right (Index Nil)) <$ userAreaView (Index Nil)) <|> (liftAff $ runExceptT $ getIndex)
  case newIndex of
    Right index -> do
      res <- userAreaView index
      case res of
        Lock -> do
          (simpleButton "Lock" true Lock) <|> (Lock <$ (liftAff $ doLogout true))
        Logout -> do
          (simpleButton "Logout" true Logout) <|> (Logout <$ (liftAff $ doLogout false))
        _ -> pure res
    Left err -> pure $ GetIndexError err

  where 
    userAreaView ix = div [Props.className "userSidebarOverlay"] [ 
        NoAction ix <$ div [Props.onClick] []
      , if isOffline then div [Props._id "userSidebar"] [
            simpleButton "Close user area" false (NoAction ix)
          , (NoAction ix) <$ exportWidget ix
          , setPinWidget Default
          , simpleButton "Lock" false Lock
          , simpleButton "Logout" false Logout
          ]
        else div [Props._id "userSidebar"] [
            simpleButton "Close user area" false (NoAction ix)
          , Loaded <$> importWidget ix
          , (NoAction ix) <$ exportWidget ix
          , setPinWidget Default
          , changePasswordWidget Default emptyChangePasswordDataForm
          , DeleteAccount <$ deleteUserWidget ix Default
          , simpleButton "Lock" false Lock
          , simpleButton "Logout" false Logout
          ]
    ]
