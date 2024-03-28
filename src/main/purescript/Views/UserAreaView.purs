module Views.UserAreaView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, header, li, li', span, text, ul)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not)
import Data.Map (Map, fromFoldable, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid ((<>))
import Data.Tuple (Tuple(..))
import DataModel.Credentials (Credentials)
import DataModel.UserVersions.User (UserPreferences)
import DataModel.WidgetState (UserAreaPage(..), UserAreaState, UserAreaSubmenu(..), ImportState)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.EnvironmentalVariables (currentCommit)
import Functions.Events (keyboardShortcut)
import Functions.State (isOffline)
import Views.ChangePasswordView (changePasswordView)
import Views.Components (Enabled(..), footerComponent)
import Views.DeleteUserView (deleteUserView)
import Views.ExportView (ExportEvent, exportView)
import Views.ImportView (importView, initialImportState)
import Views.SetPinView (PinEvent, setPinView)
import Views.UserPreferencesView (userPreferencesView)

data UserAreaEvent    = UpdateUserPreferencesEvent UserPreferences
                      | ChangePasswordEvent String
                      | SetPinEvent PinEvent
                      | DeleteAccountEvent
                      | ImportCardsEvent ImportState
                      | ExportEvent ExportEvent
                      | CloseUserAreaEvent
                      | LockEvent
                      | LogoutEvent

userAreaInitialState :: UserAreaState
userAreaInitialState = { showUserArea: false, userAreaOpenPage: None, importState: initialImportState, userAreaSubmenus: fromFoldable [(Tuple Account false), (Tuple Data false)]}

data UserAreaInternalEvent = StateUpdate UserAreaState | UserAreaEvent UserAreaEvent

userAreaView :: UserAreaState -> UserPreferences -> Credentials -> Boolean -> Widget HTML (Tuple UserAreaEvent UserAreaState)
userAreaView state@{showUserArea, userAreaOpenPage, importState, userAreaSubmenus} userPreferences credentials pinExists = do
  commitHash <- liftEffect currentCommit
  res <- (
    (div [Props._id "userPage", Props.className (if showUserArea then "open" else "closed")] [
      div [Props.onClick, Props.className "mask"] [] $> UserAreaEvent CloseUserAreaEvent
    , div [Props.className "panel"] [
        header [] [div [] [button [Props.onClick] [text "menu"]]] $> UserAreaEvent CloseUserAreaEvent
      , userAreaMenu
      , footerComponent commitHash
      ]
    , userAreaInternalView
    ])
    <> 
    ((keyboardShortcut ["l o c k"] # liftAff) $> UserAreaEvent LockEvent)
  )
  case res of
    StateUpdate updatedState -> userAreaView updatedState userPreferences credentials pinExists
    UserAreaEvent event      -> pure $ (Tuple event state)

  where
    userAreaMenu :: Widget HTML UserAreaInternalEvent
    userAreaMenu = do
      offline    <- liftEffect isOffline
      ul [Props._id "userSidebar"] [
        subMenu Account "Account" [
          subMenuElement Preferences    (Enabled $ not offline) "Preferences"
        , subMenuElement ChangePassword (Enabled $ not offline) "Passphrase"
        , subMenuElement Pin            (Enabled   true)        "Device PIN"
        , subMenuElement Delete         (Enabled $ not offline) "Delete account"
        ] <#> StateUpdate
      , subMenu Data    "Data"    [
          subMenuElement Import         (Enabled $ not offline) "Import"
        , subMenuElement Export         (Enabled $ not offline) "Export"
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

        subMenuElement :: UserAreaPage -> Enabled -> String -> Widget HTML UserAreaState
        subMenuElement userAreaPage (Enabled enabled) label = li [Props.classList [if userAreaOpenPage == userAreaPage then Just "selected" else Nothing, Just "subMenuElement"]] [
          button [Props.onClick, Props.disabled (not enabled)] [span [] [text label]] $> state {userAreaOpenPage = if userAreaOpenPage == userAreaPage then None else userAreaPage}
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
        Import          -> frame (importView          importState     <#> (UserAreaEvent <<< ImportCardsEvent))
        Export          -> frame (exportView                          <#> (UserAreaEvent <<< ExportEvent))
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