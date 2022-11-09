module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, hold, dyn)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Foldable (elem)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.List (List(..))
import Data.Semigroup ((<>))
import Data.Unit (unit, Unit)
import DataModel.AppState (AppError)
import DataModel.Index (Index(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Logout (doLogout)
import Functions.Communication.Users (getIndex)
import Views.SimpleWebComponents (simpleButton, submenu)
import OperationalWidgets.ImportWidget (importWidget)
import OperationalWidgets.ExportWidget (exportWidget)
import OperationalWidgets.ChangePasswordWidget (changePasswordWidget, emptyChangePasswordDataForm)
import OperationalWidgets.DeleteUserWidget (deleteUserWidget)
import OperationalWidgets.PinWidget (setPinWidget)

data UserAreaAction = Loaded (Either AppError Index) | Lock | Logout | DeleteAccount | NoAction Index | GetIndexError AppError

data UserAreaListVoice = Close | Export | Import | Pin | Delete | ChangePassword | VLock | VLogout | About

derive instance eqUserAreaListVoice :: Eq UserAreaListVoice

data UserAreaInternalAction = MenuAction UserAreaListVoice | UserAction UserAreaAction

userAreaWidget :: Boolean -> Widget HTML UserAreaAction
userAreaWidget isOffline = do
  newIndex <- ((Right (Index Nil)) <$ userAreaView Close (Index Nil) (div [NoAction (Index Nil) <$ Props.onClick] [])) <|> (liftAff $ runExceptT $ getIndex)
  case newIndex of
    Right index -> do
      res <- userAreaView Close index (div [NoAction index <$ Props.onClick] [])
      case res of
        Lock -> do
          (div [Lock <$ Props.onClick] []) <|> (Lock <$ (liftAff $ doLogout true))
        Logout -> do
          (div [Logout <$ Props.onClick] []) <|> (Logout <$ (liftAff $ doLogout false))
        _ -> pure res
    Left err -> pure $ GetIndexError err

  where 
    isAccountRelated :: UserAreaListVoice -> Boolean
    isAccountRelated v = elem v [Pin, Delete, ChangePassword]

    isDataRelated :: UserAreaListVoice -> Boolean
    isDataRelated v = elem v [Export, Import]

    userAreaList lastVoice = div [Props._id "userSidebar"] [
      simpleButton "Close user area" false Close
    , do
        submenu (isAccountRelated lastVoice) (simpleButton "Account" false unit) [
            simpleButton "Passphrase" isOffline ChangePassword
          , simpleButton "Device PIN" false Pin
          , simpleButton "Delete account" isOffline Delete
          ]
    , do
        submenu (isDataRelated lastVoice) (simpleButton "Data" false unit) [
          simpleButton "Export" false Export
        , simpleButton "Import" isOffline Import
        ]
    , simpleButton "About" false About
    , simpleButton "Lock" false VLock
    , simpleButton "Logout" false VLogout
    ]

    userAreaView' :: Widget HTML UserAreaListVoice -> Widget HTML UserAreaAction -> Widget HTML UserAreaInternalAction
    userAreaView' menu area = div [Props.className "userSidebarOverlay"] [ 
      UserAction <$> area
    , MenuAction <$> menu
    ]  
    
    userAreaInternalView :: Index -> UserAreaListVoice -> Widget HTML UserAreaAction
    userAreaInternalView ix choice = 
      case choice of
        Close -> pure $ NoAction ix
        Export -> div [Props.className "forUser"] [(NoAction ix) <$ exportWidget ix]
        Import -> div [Props.className "forUser"] [Loaded <$> importWidget ix]
        Pin -> div [Props.className "forUser"] [setPinWidget Default]
        Delete -> div [Props.className "forUser"] [DeleteAccount <$ deleteUserWidget ix Default]
        ChangePassword -> div [Props.className "forUser"] [changePasswordWidget Default emptyChangePasswordDataForm]
        VLock -> pure Lock
        VLogout -> pure Logout
        About -> div [Props.className "forUser"] [text "This is Clipperz"]

    userAreaView :: UserAreaListVoice -> Index -> Widget HTML UserAreaAction -> Widget HTML UserAreaAction
    userAreaView lastVoice ix area = do
      res <- userAreaView' (userAreaList lastVoice) area
      case res of
        UserAction ac -> pure $ ac
        MenuAction ac -> userAreaView ac ix (userAreaInternalView ix ac)
