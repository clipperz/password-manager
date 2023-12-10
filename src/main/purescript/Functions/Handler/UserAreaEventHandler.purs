module Functions.Handler.UserAreaEventHandler where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<#>))
import Control.Alternative ((<*))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (runExceptT, throwError)
import Data.Either (Either(..))
import Data.Function ((#), ($))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.FragmentState as Fragment
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.Communication.Users (updateUserPreferences)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, doNothing, handleOperationResult, runStep)
import Functions.Pin (makeKey)
import Functions.State (computeInitialStatelessState)
import Functions.Timer (activateTimer, stopTimer)
import Functions.User (changeUserPassword)
import Unsafe.Coerce (unsafeCoerce)
import Views.AppView (Page(..), WidgetState(..), emptyMainPageWidgetState)
import Views.CardsManagerView (CardManagerState)
import Views.LoginFormView (LoginType(..), emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (UserAreaEvent(..), UserAreaPage(..), UserAreaState)
import Web.HTML (window)
import Web.HTML.Location (reload)
import Web.HTML.Window (localStorage, location)
import Web.Storage.Storage (getItem)

handleUserAreaEvent :: UserAreaEvent -> CardManagerState -> UserAreaState -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState


handleUserAreaEvent (CloseUserAreaEvent) cardManagerState userAreaState state@{index: Just index, userPreferences: Just userPreferences, username: Just username, password: Just password} _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main { index, credentials: {username, password}, userAreaState: userAreaState {showUserArea = false, userAreaOpenPage = None}, cardManagerState, userPreferences })))


handleUserAreaEvent (UpdateUserPreferencesEvent newUserPreferences) cardManagerState userAreaState state@{index: Just index, username: Just username, password: Just password} _ = 
  do
    ProxyResponse proxy stateUpdateInfo <- runStep (updateUserPreferences state newUserPreferences) (WidgetState (spinnerOverlay "Update user preferences" White)  (Main { index, credentials: {username, password}, cardManagerState, userAreaState, userPreferences: newUserPreferences }))
    
    liftEffect $ stopTimer
    case (unwrap newUserPreferences).automaticLock of
      Left  _ -> pure unit
      Right n -> liftEffect $ activateTimer n

    pure (Tuple 
      (state {proxy = proxy, userInfoReferences = Just stateUpdateInfo.newUserInfoReferences, masterKey = Just stateUpdateInfo.newMasterKey, userPreferences = Just newUserPreferences})
      (WidgetState
        hiddenOverlayInfo
        (Main { index
              , credentials:      {username, password}
              , userPreferences:  newUserPreferences
              , userAreaState
              , cardManagerState
              }
        )
      )
    )
         
  # runExceptT
  >>= handleOperationResult state defaultErrorPage White


handleUserAreaEvent (ChangePasswordEvent newPassword) cardManagerState userAreaState state@{index: Just index, username: Just username, userPreferences: Just userPreferences} _ = 
  do
    ProxyResponse proxy userUpdateInfo <- runStep (changeUserPassword state newPassword) (WidgetState (spinnerOverlay "Update password" White) (Main { index, credentials: {username, password: newPassword}, cardManagerState, userAreaState, userPreferences }))
    pure (Tuple 
      (state {proxy = proxy, c = Just userUpdateInfo.c, p = Just userUpdateInfo.p, s = Just userUpdateInfo.s, password = Just newPassword})
      (WidgetState
        hiddenOverlayInfo
        (Main { index
              , credentials:      {username, password: newPassword}
              , userPreferences
              , userAreaState
              , cardManagerState
              }
        )
      )
    )

  # runExceptT
  >>= handleOperationResult state defaultErrorPage White

handleUserAreaEvent (SetPinEvent)                    _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "SetPinEvent")                --TODO: implement [fsolaroli - 29/11/2023]


handleUserAreaEvent (DeleteAccountEvent)             _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "DeleteAccountEvent")         --TODO: implement [fsolaroli - 29/11/2023]


handleUserAreaEvent (ImportCardsEvent)               _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ImportCardsEvent")           --TODO: implement [fsolaroli - 29/11/2023]


handleUserAreaEvent (ExportJsonEvent)                _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportJsonEvent")            --TODO: implement [fsolaroli - 29/11/2023]


handleUserAreaEvent (ExportOfflineCopyEvent)         _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportOfflineCopyEvent")     --TODO: implement [fsolaroli - 29/11/2023]


handleUserAreaEvent LockEvent _ _ {username: Just username} _ = 
  do
    state      <- liftEffect   computeInitialStatelessState
    passphrase <- liftEffect $ window >>= localStorage >>= getItem (makeKey "passphrase")
    pure $ Tuple 
            (state {username = Just username, password = passphrase})
            (WidgetState
              hiddenOverlayInfo
              (Login emptyLoginFormData { credentials = {username, password: fromMaybe "" passphrase}
                                        , loginType   = if isNothing passphrase then CredentialLogin else PinLogin
                                        }
              )
            ) 


handleUserAreaEvent LogoutEvent _ _ _ _ = liftEffect $ window >>= location >>= reload <#> unsafeCoerce


handleUserAreaEvent _ _ _ state _ = do
  throwError $ InvalidStateError (CorruptedState "State is corrupted")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage White
