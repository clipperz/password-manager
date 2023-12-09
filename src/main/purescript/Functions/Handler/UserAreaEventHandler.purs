module Functions.Handler.UserAreaEventHandler where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alternative ((<*))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (except, runExceptT)
import Data.Either (Either(..), note)
import Data.Function ((#), ($))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.FragmentState as Fragment
import DataModel.Index (emptyIndex)
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import DataModel.User (defaultUserPreferences)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.Communication.Users (updateUserPreferences)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, doNothing, handleOperationResult, runStep)
import Functions.Timer (activateTimer, stopTimer)
import Views.AppView (Page(..), WidgetState(..), emptyMainPageWidgetState)
import Views.CardsManagerView (CardManagerState)
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (UserAreaEvent(..), UserAreaPage(..), UserAreaState)

handleUserAreaEvent :: UserAreaEvent -> CardManagerState -> UserAreaState -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState

handleUserAreaEvent (CloseUserAreaEvent) cardManagerState userAreaState state@{index, userPreferences} _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main { index: fromMaybe emptyIndex index, userAreaState: userAreaState {showUserArea = false, userAreaOpenPage = None}, cardManagerState, userPreferences: fromMaybe defaultUserPreferences userPreferences })))

handleUserAreaEvent (UpdateUserPreferencesEvent newUserPreferences) cardManagerState userAreaState state@{index} _ = 
  do
    index'                                <- except $ note (InvalidStateError $ CorruptedState "index not found") index

    ProxyResponse proxy'' stateUpdateInfo <- runStep (updateUserPreferences state newUserPreferences) (WidgetState (spinnerOverlay "Update user preferences" White)  (Main { index: index', cardManagerState, userAreaState, userPreferences: newUserPreferences }))
    
    liftEffect $ stopTimer
    case (unwrap newUserPreferences).automaticLock of
      Left  _ -> pure unit
      Right n -> liftEffect $ activateTimer n

    pure (Tuple 
      (state {proxy = proxy'', userInfoReferences = Just stateUpdateInfo.newUserInfoReferences, masterKey = Just stateUpdateInfo.newMasterKey, userPreferences = Just newUserPreferences})
      (WidgetState
        hiddenOverlayInfo
        (Main { index:            index'
              , userPreferences:  newUserPreferences
              , userAreaState:    userAreaState
              , cardManagerState: cardManagerState
              }
        )
      )
    )
  
  # runExceptT
  >>= handleOperationResult state defaultErrorPage White

handleUserAreaEvent (ChangePasswordEvent)            _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ChangePasswordEvent")        --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (SetPinEvent)                    _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "SetPinEvent")                --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (DeleteAccountEvent)             _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "DeleteAccountEvent")         --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ImportCardsEvent)               _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ImportCardsEvent")           --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ExportJsonEvent)                _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportJsonEvent")            --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (ExportOfflineCopyEvent)         _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportOfflineCopyEvent")     --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (LockEvent)                      _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "LockEvent")                  --TODO: implement [fsolaroli - 29/11/2023]

handleUserAreaEvent (LogoutEvent)                   _ _  state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "LogoutEvent")                --TODO: implement [fsolaroli - 29/11/2023]
