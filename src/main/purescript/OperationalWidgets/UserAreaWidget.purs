module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, header, footer, button, li)
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
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.AppState (AppError, ProxyConnectionStatus(..))
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

data UserAreaAction = Loaded (Either AppError Index) | Lock | Logout | DeleteAccount | NoAction | GetIndexError AppError

data UserAreaListVoice = Export | Import | Pin | Delete | Preferences | ChangePassword | VLock | VLogout | About

derive instance eqUserAreaListVoice :: Eq UserAreaListVoice

data UserAreaInternalAction = MenuAction (Tuple (Array (SubmenuVoice UserAreaListVoice)) UserAreaListVoice) | UserAction (Maybe UserAreaAction) | CloseUserArea

defaultMenu :: ProxyConnectionStatus -> Array (SubmenuVoice UserAreaListVoice)
defaultMenu proxyConnectionStatus = [
  Tuple false (\b -> submenu b (simpleButton "Account" false unit) [
    li [] [simpleButtonWithId "preferencesButton"  "Preferences"     disabled  Preferences]
  , li [] [simpleButtonWithId "passphraseButton"   "Passphrase"      disabled  ChangePassword]
  , li [] [simpleButtonWithId "deviceButton"       "Device PIN"      false     Pin]
  , li [] [simpleButtonWithId "deleteButton"       "Delete account"  disabled  Delete]
  ])
, Tuple false (\b -> submenu b (simpleButton "Data" false unit) [
    li [] [simpleButtonWithId "importButton"       "Import"          disabled  Import]
  , li [] [simpleButtonWithId "exportButton"       "Export"          false     Export]
  ])
, Tuple true (\b -> submenu b (text "") [simpleButtonWithId "aboutButton"   "About"   false About])
, Tuple true (\b -> submenu b (text "") [simpleButtonWithId "lockButton"    "Lock"    false VLock])
, Tuple true (\b -> submenu b (text "") [simpleButtonWithId "logoutButton"  "Logout"  false VLogout])
]
  where
    disabled = case proxyConnectionStatus of
      ProxyOnline   -> false
      ProxyOffline  -> true

userAreaWidget :: Boolean -> ProxyConnectionStatus -> Widget HTML UserAreaAction
userAreaWidget hidden proxyConnectionStatus = userAreaView hidden (defaultMenu proxyConnectionStatus) emptyUserComponent
  where 
    emptyUserComponent :: Widget HTML (Maybe UserAreaAction)
    emptyUserComponent = (div [(Just NoAction) <$ Props.onClick, Props.className "extraFeatureContent"] [])

    userAreaList :: forall a. Array (SubmenuVoice a) -> Widget HTML (Tuple (Array (SubmenuVoice a)) a)
    userAreaList arr = complexMenu (Just "userSidebar") Nothing arr

    userAreaView :: Boolean -> Array (SubmenuVoice UserAreaListVoice) -> Widget HTML (Maybe UserAreaAction) -> Widget HTML UserAreaAction
    userAreaView hidden' arr area = do
      let userPageClassName = if hidden' then "closed" else "open"
      res <- div [Props._id "userPage", Props.className userPageClassName] [
        CloseUserArea <$ div [Props.onClick, Props.className "mask"] [], --TODO - doesn't work
        div [Props.className "panel"] [
          CloseUserArea <$ header [] [div [] [button [Props.onClick] [text "menu"]]],
          div [] [MenuAction <$> (userAreaList arr)],
          footer [] [text "footer"]
        ],
        UserAction <$> area
      ]
      case res of
        CloseUserArea -> userAreaView true arr emptyUserComponent
        UserAction maybeUserAreaAction -> case maybeUserAreaAction of
          Just ac -> pure $ ac 
          Nothing -> userAreaView false arr emptyUserComponent
        MenuAction (Tuple newMenus ac) -> userAreaView false newMenus (userAreaInternalView ac)

    userAreaInternalView :: UserAreaListVoice -> Widget HTML (Maybe UserAreaAction)
    userAreaInternalView choice = 
      case choice of
        Export          -> frame (NoAction      <$  exportWidget)
        Import          -> frame (Loaded        <$> importWidget)
        Pin             -> frame (setPinWidget Default)
        Delete          -> frame (DeleteAccount <$  deleteUserWidget)
        Preferences     -> frame (NoAction  <$ userPreferencesWidget Default)
        ChangePassword  -> frame (changePasswordWidget Default emptyChangePasswordDataForm)
        VLock           -> pure $ Just Lock
        VLogout         -> pure $ Just Logout
        About           -> frame (text "This is Clipperz")

      where
        frame :: Widget HTML UserAreaAction -> Widget HTML (Maybe UserAreaAction)
        frame c = div [Props.classList $ Just <$> ["extraFeatureContent", "open"]] [
          Nothing <$ header [] [div [] [button [Props.onClick] [text "close"]]],
          Just <$> c
        ]
