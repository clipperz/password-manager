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
import Data.Array (toUnfoldable, fromFoldable)
import Data.Either (Either(..))
import Data.Eq (class Eq)
import Data.Foldable (elem)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Unit (unit, Unit)
import DataModel.AppState (AppError)
import DataModel.Index (Index(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Logout (doLogout)
import Functions.Communication.Users (getIndex)
import Views.SimpleWebComponents (simpleButton, submenu, complexMenu, SubmenuVoice)
import OperationalWidgets.ImportWidget (importWidget)
import OperationalWidgets.ExportWidget (exportWidget)
import OperationalWidgets.ChangePasswordWidget (changePasswordWidget, emptyChangePasswordDataForm)
import OperationalWidgets.UserPreferencesWidget (userPreferencesWidget)
import OperationalWidgets.DeleteUserWidget (deleteUserWidget)
import OperationalWidgets.PinWidget (setPinWidget)

data UserAreaAction = Loaded (Either AppError Index) | Lock | Logout | DeleteAccount | NoAction Index | GetIndexError AppError

data UserAreaListVoice = Close | Export | Import | Pin | Delete | Preferences | ChangePassword | VLock | VLogout | About

derive instance eqUserAreaListVoice :: Eq UserAreaListVoice

data UserAreaInternalAction = MenuAction (Tuple (Array (SubmenuVoice UserAreaListVoice)) UserAreaListVoice) | UserAction UserAreaAction

defaultMenu = \isOffline -> [
  Tuple true (\b -> submenu b (text "") [simpleButton "Close user area" false Close])
, Tuple false (\b -> submenu b (simpleButton "Account" false unit) [
    simpleButton "Preferences" isOffline Preferences
  , simpleButton "Passphrase" isOffline ChangePassword
  , simpleButton "Device PIN" false Pin
  , simpleButton "Delete account" isOffline Delete
  ])
, Tuple false (\b -> submenu b (simpleButton "Data" false unit) [
    simpleButton "Export" false Export
  , simpleButton "Import" isOffline Import
  ])
, Tuple true (\b -> submenu b (text "") [simpleButton "About" false About])
, Tuple true (\b -> submenu b (text "") [simpleButton "Lock" false VLock])
, Tuple true (\b -> submenu b (text "") [simpleButton "Logout" false VLogout])
]

userAreaWidget :: Boolean -> Widget HTML UserAreaAction
userAreaWidget isOffline = do
  newIndex <- ((Right (Index Nil)) <$ userAreaView (defaultMenu isOffline) (Index Nil) (div [NoAction (Index Nil) <$ Props.onClick] [])) <|> (liftAff $ runExceptT $ getIndex)
  case newIndex of
    Right index -> do
      res <- userAreaView (defaultMenu isOffline) index (div [NoAction index <$ Props.onClick] [])
      case res of
        Lock -> do
          (div [Lock <$ Props.onClick] []) <|> (Lock <$ (liftAff $ doLogout true))
        Logout -> do
          (div [Logout <$ Props.onClick] []) <|> (Logout <$ (liftAff $ doLogout false))
        _ -> pure res
    Left err -> pure $ GetIndexError err

  where 
    userAreaList arr = complexMenu (Just "userSidebar") Nothing arr

    userAreaView' :: Widget HTML (Tuple (Array (SubmenuVoice UserAreaListVoice)) UserAreaListVoice) -> Widget HTML UserAreaAction -> Widget HTML UserAreaInternalAction
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
        Preferences -> div [Props.className "forUser"] [(NoAction ix) <$ userPreferencesWidget Default]
        ChangePassword -> div [Props.className "forUser"] [changePasswordWidget Default emptyChangePasswordDataForm]
        VLock -> pure Lock
        VLogout -> pure Logout
        About -> div [Props.className "forUser"] [text "This is Clipperz"]

    userAreaView :: Array (SubmenuVoice UserAreaListVoice) -> Index -> Widget HTML UserAreaAction -> Widget HTML UserAreaAction
    userAreaView arr ix area = do
      res <- userAreaView' (userAreaList arr) area
      case res of
        UserAction ac -> pure $ ac
        MenuAction (Tuple newMenus ac) -> userAreaView newMenus ix (userAreaInternalView ix ac)

    updateMenu :: Array (SubmenuVoice UserAreaListVoice) -> Array Boolean -> Array (SubmenuVoice UserAreaListVoice)
    updateMenu a a' = fromFoldable $ updateMenu' (toUnfoldable a) (toUnfoldable a')
      where
        updateMenu' Nil _   = Nil
        updateMenu' _   Nil = Nil
        updateMenu' (Cons (Tuple b f) l) (Cons b' l') = Cons (Tuple b' f) (updateMenu' l l')

