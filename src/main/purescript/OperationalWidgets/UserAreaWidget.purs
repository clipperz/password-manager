module OperationalWidgets.UserAreaWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, header, li, text)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Either (Either)
import Data.Eq (class Eq, (==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.AppState (AppError, ProxyConnectionStatus(..))
import DataModel.Index (Index)
import DataModel.WidgetState (WidgetState(..))
import Effect.Class (liftEffect)
import Functions.EnvironmentalVariables (currentCommit)
import OperationalWidgets.ChangePasswordWidget (changePasswordWidget, emptyChangePasswordDataForm)
import OperationalWidgets.DeleteUserWidget (deleteUserWidget)
import OperationalWidgets.ExportWidget (exportWidget)
import OperationalWidgets.ImportWidget (importWidget)
import OperationalWidgets.PinWidget (setPinWidget)
import OperationalWidgets.UserPreferencesWidget (userPreferencesWidget)
import Views.Components (footerComponent)
import Views.SimpleWebComponents (simpleButton, submenu, complexMenu, SubmenuVoice)

data UserAreaAction = Loaded (Either AppError Index) | Lock | Logout | DeleteAccount | NoAction | GetIndexError AppError

data UserAreaListVoice = Export | Import | Pin | Delete | Preferences | ChangePassword | VLock | VLogout | About | None

derive instance eqUserAreaListVoice :: Eq UserAreaListVoice

data UserAreaInternalAction = MenuAction (Tuple (Array (SubmenuVoice UserAreaListVoice)) UserAreaListVoice) | UserAction (Maybe UserAreaAction) | CloseUserArea

defaultMenu :: ProxyConnectionStatus -> Array (SubmenuVoice UserAreaListVoice)
defaultMenu proxyConnectionStatus = [
  Tuple false (\b -> \area -> submenu b (simpleButton "account" "Account" false unit) [
    submenuItem "preferencesButton"  "Preferences"     disabled  Preferences area
  , submenuItem "passphraseButton"   "Passphrase"      disabled  ChangePassword area
  , submenuItem "deviceButton"       "Device PIN"      false     Pin area
  , submenuItem "deleteButton"       "Delete account"  disabled  Delete area
  ])
, Tuple false (\b -> \area -> submenu b (simpleButton "data" "Data" false unit) [
    submenuItem "importButton"       "Import"          disabled  Import area
  , submenuItem "exportButton"       "Export"          false     Export area
  ])
, Tuple true (\b -> \_ -> submenu b (text "") [simpleButton "aboutButton"   "About"   false About])
, Tuple true (\b -> \_ -> submenu b (text "") [simpleButton "lockButton"    "Lock"    false VLock])
, Tuple true (\b -> \_ -> submenu b (text "") [simpleButton "logoutButton"  "Logout"  false VLogout])
]
  where
    disabled = case proxyConnectionStatus of
      ProxyOnline   -> false
      ProxyOffline  -> true
  
    submenuItem :: String -> String -> Boolean -> UserAreaListVoice -> UserAreaListVoice -> Widget HTML UserAreaListVoice
    submenuItem className label disable area currentArea = 
      li [Props.classList [if area == currentArea then Just "selected" else Nothing]] [
        button [area <$ Props.onClick, Props.disabled disable, Props.className className] [text label]
      ]

userAreaWidget :: Boolean -> ProxyConnectionStatus -> Widget HTML UserAreaAction
userAreaWidget hidden proxyConnectionStatus = userAreaView hidden (defaultMenu proxyConnectionStatus) None
  where 
    emptyUserComponent :: Widget HTML (Maybe UserAreaAction)
    emptyUserComponent = (div [(Just NoAction) <$ Props.onClick, Props.className "extraFeatureContent"] [])

    userAreaList :: forall a. Array (SubmenuVoice a) -> a -> Widget HTML (Tuple (Array (SubmenuVoice a)) a)
    userAreaList arr area = complexMenu (Just "userSidebar") Nothing arr area

    userAreaView :: Boolean -> Array (SubmenuVoice UserAreaListVoice) -> UserAreaListVoice -> Widget HTML UserAreaAction
    userAreaView hidden' arr area = do
      let userPageClassName = if hidden' then "closed" else "open"
      commitHash <- liftEffect $ currentCommit
      res <- div [Props._id "userPage", Props.className userPageClassName] [
        CloseUserArea <$ div [Props.onClick, Props.className "mask"] [], --TODO - doesn't work
        div [Props.className "panel"] [
          CloseUserArea <$ header [] [div [] [button [Props.onClick] [text "menu"]]],
          div [] [MenuAction <$> (userAreaList arr area)],
          footerComponent commitHash
        ],
        UserAction <$> (userAreaInternalView area)
      ]
      case res of
        CloseUserArea -> userAreaView true arr None
        UserAction maybeUserAreaAction -> case maybeUserAreaAction of
          Just ac -> pure $ ac 
          Nothing -> userAreaView false arr None
        MenuAction (Tuple newMenus ac) -> userAreaView false newMenus ac

    userAreaInternalView :: UserAreaListVoice -> Widget HTML (Maybe UserAreaAction)
    userAreaInternalView choice = 
      case choice of
        Export          -> frame (NoAction      <$  exportWidget)
        Import          -> frame (Loaded        <$> importWidget)
        Pin             -> frame (setPinWidget Default)
        Delete          -> frame (DeleteAccount <$  deleteUserWidget)
        Preferences     -> frame (NoAction      <$  userPreferencesWidget Default)
        ChangePassword  -> frame (changePasswordWidget Default emptyChangePasswordDataForm)
        VLock           -> pure $ Just Lock
        VLogout         -> pure $ Just Logout
        About           -> frame (text "This is Clipperz")
        None            -> emptyUserComponent

      where
        frame :: Widget HTML UserAreaAction -> Widget HTML (Maybe UserAreaAction)
        frame c = div [Props.classList $ Just <$> ["extraFeatureContent", "open"]] [
          Nothing <$ header [] [div [] [button [Props.onClick] [text "close"]]],
          Just <$> c
        ]
