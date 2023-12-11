module Views.UserAreaView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, header, li, li', span, text, ul)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Eq (class Eq, (==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (class Ord)
import Data.Tuple (Tuple(..))
import DataModel.Credentials (Credentials)
import DataModel.User (UserPreferences)
import Effect.Class (liftEffect)
import Functions.EnvironmentalVariables (currentCommit)
import OperationalWidgets.ExportWidget (exportWidget)
import OperationalWidgets.ImportWidget (importWidget)
import Views.ChangePasswordView (changePasswordView)
import Views.Components (footerComponent)
import Views.DeleteUserView (deleteUserView)
import Views.SetPinView (PinEvent, setPinView)
import Views.UserPreferencesView (userPreferencesView)

data UserAreaEvent    = UpdateUserPreferencesEvent UserPreferences
                      | ChangePasswordEvent String
                      | SetPinEvent PinEvent
                      | DeleteAccountEvent
                      | ImportCardsEvent -- List Card ??
                      | ExportJsonEvent -- ??
                      | ExportOfflineCopyEvent -- ??
                      | CloseUserAreaEvent
                      | LockEvent
                      | LogoutEvent

type UserAreaState = {
  showUserArea     :: Boolean
, userAreaOpenPage :: UserAreaPage
, userAreaSubmenus :: Map UserAreaSubmenu Boolean
}

userAreaInitialState :: UserAreaState
userAreaInitialState = { showUserArea: false, userAreaOpenPage: None, userAreaSubmenus: fromFoldable [(Tuple Account false), (Tuple Data false)]}

data UserAreaPage = Export | Import | Pin | Delete | Preferences | ChangePassword | About | None
derive instance eqUserAreaPage :: Eq UserAreaPage

data UserAreaSubmenu = Account | Data
derive instance  eqUserAreaSubmenus :: Eq  UserAreaSubmenu
derive instance ordUserAreaSubmenus :: Ord UserAreaSubmenu

data UserAreaInternalEvent = StateUpdate UserAreaState | UserAreaEvent UserAreaEvent

userAreaView :: UserAreaState -> UserPreferences -> Credentials -> Boolean -> Widget HTML (Tuple UserAreaEvent UserAreaState)
userAreaView state@{showUserArea, userAreaOpenPage, userAreaSubmenus} userPreferences credentials pinExists = do
  commitHash <- liftEffect $ currentCommit
  res <- div [Props._id "userPage", Props.className (if showUserArea then "open" else "closed")] [
    div [Props.onClick, Props.className "mask"] [] $> UserAreaEvent CloseUserAreaEvent
  , div [Props.className "panel"] [
      header [] [div [] [button [Props.onClick] [text "menu"]]] $> UserAreaEvent CloseUserAreaEvent
    , userAreaMenu
    , footerComponent commitHash
    ]
  , userAreaInternalView
  ]
  case res of
    StateUpdate updatedState -> userAreaView updatedState userPreferences credentials pinExists
    UserAreaEvent event      -> pure $ (Tuple event state)

  where
    userAreaMenu :: Widget HTML UserAreaInternalEvent
    userAreaMenu = ul [Props._id "userSidebar"] [
      subMenu Account "Account" [
        subMenuElement Preferences    "Preferences"
      , subMenuElement ChangePassword "Passphrase"
      , subMenuElement Pin            "Device PIN"
      , subMenuElement Delete         "Delete account"
      ] <#> StateUpdate
    , subMenu Data    "Data"    [
        subMenuElement Import         "Import"
      , subMenuElement Export         "Export"
      ] <#> StateUpdate
    , li' [a      [Props.className "link", Props.href "/about/app", Props.target "_blank"] [span [] [text "About"]]]
    , li' [button [Props.onClick, Props._id "lockButton"]                                  [span [] [text "Lock"]]]   $> UserAreaEvent LockEvent
    , li' [button [Props.onClick]                                                          [span [] [text "Logout"]]] $> UserAreaEvent LogoutEvent
    ]

      where
        subMenu :: UserAreaSubmenu -> String -> Array (Widget HTML UserAreaState) -> Widget HTML UserAreaState
        subMenu userAreaSubmenu label subMenuElements = li [] [
          button [Props.onClick] [span [] [text label]] $> state {userAreaSubmenus = invertSubmenuValue userAreaSubmenu}
        , ul [Props.classList [Just "userSidebarSubitems", if isSubmenuOpen userAreaSubmenu then Nothing else Just "hidden"]]
            subMenuElements
        ]

        subMenuElement :: UserAreaPage -> String -> Widget HTML UserAreaState
        subMenuElement userAreaPage label = li [Props.classList [if userAreaOpenPage == userAreaPage then Just "selected" else Nothing]] [
          button [Props.onClick] [span [] [text label]] $> state {userAreaOpenPage = if userAreaOpenPage == userAreaPage then None else userAreaPage}
        ]
        
        invertSubmenuValue :: UserAreaSubmenu -> Map UserAreaSubmenu Boolean
        invertSubmenuValue userAreaSubmenu = insert userAreaSubmenu (not $ isSubmenuOpen userAreaSubmenu) userAreaSubmenus

        isSubmenuOpen :: UserAreaSubmenu -> Boolean
        isSubmenuOpen userAreaSubmenu = fromMaybe false $ lookup userAreaSubmenu userAreaSubmenus

    userAreaInternalView :: Widget HTML UserAreaInternalEvent
    userAreaInternalView = 
      case userAreaOpenPage of
        Preferences     -> frame (userPreferencesView userPreferences <#> (UserAreaEvent <<< UpdateUserPreferencesEvent))
        ChangePassword  -> frame (changePasswordView  credentials     <#> (UserAreaEvent <<< ChangePasswordEvent))
        Pin             -> frame (setPinView          pinExists       <#> (UserAreaEvent <<< SetPinEvent))
        Delete          -> frame (deleteUserView      credentials      $> (UserAreaEvent     DeleteAccountEvent))
        Import          -> frame (importWidget $> UserAreaEvent ImportCardsEvent)
        Export          -> frame (exportWidget $> UserAreaEvent ExportJsonEvent)
        About           -> frame (text "This is Clipperz")
        None            -> emptyUserComponent

      where
        frame :: Widget HTML UserAreaInternalEvent -> Widget HTML UserAreaInternalEvent
        frame c = div [Props.classList $ Just <$> ["extraFeatureContent", "open"]] [
          header [] [div [] [button [Props.onClick] [text "close"]]] $> (StateUpdate $ state {userAreaOpenPage = None})
        , c
        ]

        emptyUserComponent :: forall a. Widget HTML a
        emptyUserComponent = (div [Props.className "extraFeatureContent"] [])