module Functions.Handler.UserAreaEventHandler where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alternative ((<*))
import Data.Function (($))
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import DataModel.FragmentState as Fragment
import DataModel.Index (emptyIndex)
import DataModel.Password (standardPasswordGeneratorSettings)
import DataModel.StatelessAppState (StatelessAppState)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.Handler.GenericHandlerFunctions (OperationState, doNothing)
import Views.AppView (Page(..), WidgetState(..), emptyMainPageWidgetState)
import Views.CardsManagerView (CardManagerState)
import Views.OverlayView (hiddenOverlayInfo)
import Views.UserAreaView (UserAreaEvent(..), UserAreaState)


handleUserAreaEvent :: UserAreaEvent -> CardManagerState -> UserAreaState -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState

handleUserAreaEvent (CloseUserAreaEvent) cardManagerState userAreaState state@{index, userPreferences} _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main { index: fromMaybe emptyIndex index, userAreaState: userAreaState {showUserArea = false}, cardManagerState, userPasswordGeneratorSettings: maybe standardPasswordGeneratorSettings ((\up -> (unwrap up).passwordGeneratorSettings)) userPreferences })))

handleUserAreaEvent (UpdateUserPreferencesEvent)     _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "UpdateUserPreferencesEvent") --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ChangePasswordEvent)            _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ChangePasswordEvent")        --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (SetPinEvent)                    _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "SetPinEvent")                --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (DeleteAccountEvent)             _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "DeleteAccountEvent")         --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ImportCardsEvent)               _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ImportCardsEvent")           --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ExportJsonEvent)                _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportJsonEvent")            --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ExportOfflineCopyEvent)         _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportOfflineCopyEvent")     --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (LockEvent)                      _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "LockEvent")                  --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (LogoutEvent)                   _ _  state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "LogoutEvent")                --TODO: implement [fsolaroli - 29/11/2023]
