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
import Views.AppView (Page(..), UserAreaEvent(..), WidgetState(..), emptyMainPageWidgetState)
import Views.OverlayView (hiddenOverlayInfo)


handleUserAreaEvent :: UserAreaEvent -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState

handleUserAreaEvent (CloseUserArea cardManagerState) state@{index, userPreferences} _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main { index: fromMaybe emptyIndex index, showUserArea: false, cardManagerState, userPasswordGeneratorSettings: maybe standardPasswordGeneratorSettings ((\up -> (unwrap up).passwordGeneratorSettings)) userPreferences })))

handleUserAreaEvent (UpdateUserPreferencesEvent)     state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "UpdateUserPreferencesEvent") --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ChangePasswordEvent)            state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ChangePasswordEvent")        --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (SetPinEvent)                    state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "SetPinEvent")                --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (DeleteAccountEvent)             state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "DeleteAccountEvent")         --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ImportCardsEvent)               state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ImportCardsEvent")           --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ExportJsonEvent)                state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportJsonEvent")            --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ExportOfflineCopyEvent)         state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportOfflineCopyEvent")     --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (LockEvent)                      state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "LockEvent")                  --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (LogoutEvent)                    state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "LogoutEvent")                --TODO: implement [fsolaroli - 29/11/2023]
