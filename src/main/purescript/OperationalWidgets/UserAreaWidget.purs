module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.AppState (AppError)
import DataModel.Index (Index(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Logout (doLogout)
import Functions.Communication.Users (getIndex)
import Views.SimpleWebComponents (simpleButton, simpleButtonWithId, submenu, complexMenu, SubmenuVoice)
import OperationalWidgets.ImportWidget (importWidget)
import OperationalWidgets.ExportWidget (exportWidget)
import OperationalWidgets.ChangePasswordWidget (changePasswordWidget, emptyChangePasswordDataForm)
import OperationalWidgets.UserPreferencesWidget (userPreferencesWidget)
import OperationalWidgets.DeleteUserWidget (deleteUserWidget)
import OperationalWidgets.PinWidget (setPinWidget)

data UserAreaAction = Loaded (Either AppError Index) | Lock | Logout | DeleteAccount | NoAction Index | GetIndexError AppError

data UserAreaListVoice = Export | Import | Pin | Delete | Preferences | ChangePassword | VLock | VLogout | About

derive instance eqUserAreaListVoice :: Eq UserAreaListVoice

data UserAreaInternalAction = MenuAction (Tuple (Array (SubmenuVoice UserAreaListVoice)) UserAreaListVoice) | UserAction UserAreaAction | OpenClose

defaultMenu :: Boolean -> Array (SubmenuVoice UserAreaListVoice)
defaultMenu = \isOffline -> [
  Tuple false (\b -> submenu b (simpleButton "Account" false unit) [
    simpleButtonWithId "preferencesButton" "Preferences" isOffline Preferences
  , simpleButtonWithId "passphraseButton" "Passphrase" isOffline ChangePassword
  , simpleButtonWithId "deviceButton" "Device PIN" false Pin
  , simpleButtonWithId "deleteButton" "Delete account" isOffline Delete
  ])
, Tuple false (\b -> submenu b (simpleButton "Data" false unit) [
    simpleButtonWithId "exportButton" "Export" false Export
  , simpleButtonWithId "importButton" "Import" isOffline Import
  ])
, Tuple true (\b -> submenu b (text "") [simpleButtonWithId "aboutButton" "About" false About])
, Tuple true (\b -> submenu b (text "") [simpleButtonWithId "lockButton" "Lock" false VLock])
, Tuple true (\b -> submenu b (text "") [simpleButtonWithId "logoutButton" "Logout" false VLogout])
]

userAreaWidget :: Boolean -> Boolean -> Widget HTML UserAreaAction
userAreaWidget hidden isOffline = do
  newIndex <- ((Right (Index Nil)) <$ userAreaView hidden (defaultMenu isOffline) (Index Nil) (div [NoAction (Index Nil) <$ Props.onClick] [])) <|> (liftAff $ runExceptT $ getIndex)
  case newIndex of
    Right index -> do
      res <- userAreaView hidden (defaultMenu isOffline) index (div [NoAction index <$ Props.onClick] [])
      case res of
        Lock -> do
          (div [Lock <$ Props.onClick] []) <|> (Lock <$ (liftAff $ doLogout true))
        Logout -> do
          (div [Logout <$ Props.onClick] []) <|> (Logout <$ (liftAff $ doLogout false))
        _ -> pure res
    Left err -> pure $ GetIndexError err

  where 
    userAreaList arr = complexMenu (Just "userSidebar") Nothing arr

    userAreaView' :: Boolean -> Widget HTML (Tuple (Array (SubmenuVoice UserAreaListVoice)) UserAreaListVoice) -> Widget HTML UserAreaAction -> Widget HTML UserAreaInternalAction
    userAreaView' hidden menu area = 
      let hiddenClass = if hidden then " hidden" else ""
      in div [Props.className ("userSidebarOverlay" <> hiddenClass)] [ 
        UserAction <$> area
      , MenuAction <$> menu
      ]  
    
    userAreaInternalView :: Index -> UserAreaListVoice -> Widget HTML UserAreaAction
    userAreaInternalView ix choice = 
      case choice of
        Export -> div [Props.className "forUser"] [(NoAction ix) <$ exportWidget]
        Import -> div [Props.className "forUser"] [Loaded <$> importWidget]
        Pin -> div [Props.className "forUser"] [setPinWidget Default]
        Delete -> div [Props.className "forUser"] [DeleteAccount <$ deleteUserWidget]
        Preferences -> div [Props.className "forUser"] [(NoAction ix) <$ userPreferencesWidget Default]
        ChangePassword -> div [Props.className "forUser"] [changePasswordWidget Default emptyChangePasswordDataForm]
        VLock -> pure Lock
        VLogout -> pure Logout
        About -> div [Props.className "forUser"] [text "This is Clipperz"]

    userAreaView :: Boolean -> Array (SubmenuVoice UserAreaListVoice) -> Index -> Widget HTML UserAreaAction -> Widget HTML UserAreaAction
    userAreaView hidden arr ix area = do
      let userPageClassName = if hidden then "closed" else "open"
      let openCloseLabel = (if hidden then "Open" else "Close") <> " user area"
      res <- div [Props._id "userPage", Props.className userPageClassName] [
        div [Props.className "userSidebarTop"] [simpleButton openCloseLabel false OpenClose]
      , userAreaView' hidden (userAreaList arr) area
      ]
      case res of
        OpenClose -> userAreaView (not hidden) arr ix area
        UserAction ac -> pure $ ac
        MenuAction (Tuple newMenus ac) -> userAreaView false newMenus ix (userAreaInternalView ix ac)
