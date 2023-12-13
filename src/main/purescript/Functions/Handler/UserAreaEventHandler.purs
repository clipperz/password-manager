module Functions.Handler.UserAreaEventHandler
  ( handleUserAreaEvent
  )
  where

import Affjax.ResponseFormat as RF
import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt (($>), (<#>), (<$>))
import Control.Alternative ((<*))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Either (Either(..))
import Data.Function ((#), ($))
import Data.HTTP.Method (Method(..))
import Data.HexString (hex)
import Data.List (foldl)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Credentials (emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.Index (Index(..))
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState, discardResult)
import DataModel.User (UserInfoReferences(..))
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.Communication.Blobs (deleteBlobWithReference)
import Functions.Communication.Cards (deleteCard)
import Functions.Communication.StatelessBackend (manageGenericRequest)
import Functions.Communication.Users (deleteUserCard, deleteUserInfo, updateUserPreferences)
import Functions.Handler.CardManagerEventHandler (getCardSteps)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, doNothing, handleOperationResult, runStep)
import Functions.Pin (deleteCredentials, makeKey, saveCredentials)
import Functions.State (resetState)
import Functions.Timer (activateTimer, stopTimer)
import Functions.User (changeUserPassword)
import Views.AppView (Page(..), WidgetState(..), emptyMainPageWidgetState)
import Views.CardsManagerView (CardManagerState)
import Views.LoginFormView (LoginType(..), emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SetPinView (PinEvent(..))
import Views.UserAreaView (UserAreaEvent(..), UserAreaPage(..), UserAreaState)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

handleUserAreaEvent :: UserAreaEvent -> CardManagerState -> UserAreaState -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState


handleUserAreaEvent (CloseUserAreaEvent) cardManagerState userAreaState state@{index: Just index, userPreferences: Just userPreferences, username: Just username, password: Just password, pinEncryptedPassword} _ = 
  doNothing (Tuple 
              state
              (WidgetState
                hiddenOverlayInfo
                (Main { index
                      , credentials:   {username, password}
                      , pinExists:     isJust pinEncryptedPassword
                      , userAreaState: userAreaState {showUserArea = false, userAreaOpenPage = None}
                      , cardManagerState
                      , userPreferences
                      }
                )
              )
            )


handleUserAreaEvent (UpdateUserPreferencesEvent newUserPreferences) cardManagerState userAreaState state@{index: Just index, username: Just username, password: Just password, pinEncryptedPassword} _ = 
  do
    ProxyResponse proxy stateUpdateInfo <- runStep (updateUserPreferences state newUserPreferences) (WidgetState (spinnerOverlay "Update user preferences" White) page)
    
    liftEffect $ stopTimer
    case (unwrap newUserPreferences).automaticLock of
      Left  _ -> pure unit
      Right n -> liftEffect $ activateTimer n

    pure (Tuple 
      (state {proxy = proxy, userInfoReferences = Just stateUpdateInfo.newUserInfoReferences, masterKey = Just stateUpdateInfo.newMasterKey, userPreferences = Just newUserPreferences})
      (WidgetState hiddenOverlayInfo page)
    )
         
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White

  where
    page = Main { index
                , credentials:      {username, password}
                , pinExists:        isJust pinEncryptedPassword
                , userPreferences:  newUserPreferences
                , userAreaState
                , cardManagerState
                }


handleUserAreaEvent (ChangePasswordEvent newPassword) cardManagerState userAreaState state@{index: Just index, username: Just username, password: Just password, userPreferences: Just userPreferences, pinEncryptedPassword} _ = 
  do
    ProxyResponse proxy userUpdateInfo <- runStep (changeUserPassword state newPassword) (WidgetState (spinnerOverlay "Update password" White) page)
    pure (Tuple 
      (state {proxy = proxy, c = Just userUpdateInfo.c, p = Just userUpdateInfo.p, s = Just userUpdateInfo.s, password = Just newPassword})
      (WidgetState hiddenOverlayInfo page)
    )

  # runExceptT
  >>= handleOperationResult state errorPage true White

  where
    page      = Main { index
                     , credentials:      {username, password: newPassword}
                     , pinExists:        isJust pinEncryptedPassword
                     , userPreferences
                     , userAreaState
                     , cardManagerState
                     }

    errorPage = Main { index
                     , credentials:      {username, password}
                     , pinExists:        isJust pinEncryptedPassword
                     , userPreferences
                     , userAreaState
                     , cardManagerState
                     }

handleUserAreaEvent (SetPinEvent pinAction) cardManagerState userAreaState state@{index: Just index, username: Just username, password: Just password, userPreferences: Just userPreferences} _ =
  do
    storage <- liftEffect $ window >>= localStorage
    pinEncryptedPassword <- runStep (case pinAction of
                                      Reset      -> (liftEffect $ deleteCredentials storage)  $> Nothing
                                      SetPin pin -> (saveCredentials state pin storage)      <#> Just
                                    ) (WidgetState
                                        (spinnerOverlay (case pinAction of
                                                          Reset    -> "Reset PIN"
                                                          SetPin _ -> "Set PIN") 
                                                        White)
                                        page
                                      )
    pure (Tuple 
            (state {pinEncryptedPassword = pinEncryptedPassword})
            (WidgetState
              hiddenOverlayInfo
              page
            )
          )
  
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White
  
  where 
    page = Main { index
                , credentials: {username, password}
                , pinExists: case pinAction of
                              Reset    -> false
                              SetPin _ -> true
                , cardManagerState
                , userAreaState
                , userPreferences
                }


handleUserAreaEvent DeleteAccountEvent cardManagerState userAreaState state@{hash: hashFunc, username: Just username, password: Just password, index: Just index, userPreferences: Just userPreferences, userInfoReferences: Just (UserInfoReferences {indexReference, preferencesReference}), c: Just c, masterKey: Just masterKey, pinEncryptedPassword} _ =
  do
    ProxyResponse proxy'     _ <- deleteCardsSteps state index page
    ProxyResponse proxy''    _ <- runStep (deleteBlobWithReference {hashFunc, proxy: proxy'}   (unwrap indexReference).reference      ) (WidgetState (spinnerOverlay "Delete Index"       White) page)
    ProxyResponse proxy'''   _ <- runStep (deleteBlobWithReference {hashFunc, proxy: proxy''}  (unwrap preferencesReference).reference) (WidgetState (spinnerOverlay "Delete Preferences" White) page)
    ProxyResponse proxy''''  _ <- runStep (deleteUserInfo          {hashFunc, proxy: proxy'''}  masterKey                             ) (WidgetState (spinnerOverlay "Delete User Info"   White) page)
    ProxyResponse proxy''''' _ <- runStep (deleteUserCard          {hashFunc, proxy: proxy''''} c                                     ) (WidgetState (spinnerOverlay "Delete User Card"   White) page)
    _                          <- liftEffect $ window >>= localStorage >>= deleteCredentials
    logoutSteps (state {proxy = proxy''''', username = Nothing}) "Logout" page
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White

  where 
    page = Main { index
                , credentials: {username, password}
                , pinExists:   isJust pinEncryptedPassword
                , cardManagerState
                , userAreaState
                , userPreferences
                }

handleUserAreaEvent (ImportCardsEvent)               _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ImportCardsEvent")           --TODO: implement [fsolaroli - 29/11/2023]


handleUserAreaEvent (ExportJsonEvent)                _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportJsonEvent")            --TODO: implement [fsolaroli - 29/11/2023]


handleUserAreaEvent (ExportOfflineCopyEvent)         _ _ state         _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main emptyMainPageWidgetState))) <* (liftEffect $ log "ExportOfflineCopyEvent")     --TODO: implement [fsolaroli - 29/11/2023]


handleUserAreaEvent LockEvent cardManagerState userAreaState state@{username: Just username, password: Just password, index: Just index, userPreferences: Just userPreferences, pinEncryptedPassword} _ = 
  logoutSteps state "Lock" page
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White
  
  where
    page = Main { index
                , credentials:      {username, password}
                , pinExists:        isJust pinEncryptedPassword
                , userPreferences
                , userAreaState
                , cardManagerState
                }

handleUserAreaEvent LogoutEvent cardManagerState userAreaState state@{index: Just index, username: Just username, password: Just password, userPreferences: Just userPreferences, pinEncryptedPassword} _ = 
  logoutSteps (state {username = Nothing}) "Logout" page
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White

  where
    page = Main { index
                , credentials:      {username, password}
                , pinExists:        isJust pinEncryptedPassword
                , userPreferences
                , userAreaState
                , cardManagerState
                }

handleUserAreaEvent _ _ _ state _ = do
  throwError $ InvalidStateError (CorruptedState "State is corrupted")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White

-- ===================================================================================================

logoutSteps :: StatelessAppState -> String -> Page -> ExceptT AppError (Widget HTML) OperationState
logoutSteps state@{username, hash: hashFunc, proxy} message page =
  do
    passphrase <- runStep (do 
                            _   <- manageGenericRequest {hashFunc, proxy} "logout" POST Nothing RF.string
                            res <- liftEffect $ window >>= localStorage >>= getItem (makeKey "passphrase")
                            pure res
                          ) (WidgetState (spinnerOverlay message White) page)
    
    pure $ Tuple 
            ((resetState state) {username = username, pinEncryptedPassword = hex <$> passphrase})
            (WidgetState
              hiddenOverlayInfo
              (Login emptyLoginFormData { credentials = emptyCredentials {username = fromMaybe "" username}
                                        , loginType   = if isNothing passphrase then CredentialLogin else PinLogin
                                        }
              )
            ) 

deleteCardsSteps :: StatelessAppState -> Index -> Page -> ExceptT AppError (Widget HTML) (ProxyResponse Unit)
deleteCardsSteps state@{hash: hashFunc, proxy} (Index list) page =
  foldl (\proxyResponse entry -> do
    ProxyResponse proxy' _      <- proxyResponse
    Tuple {proxy: proxy''} card <- getCardSteps (state {proxy = proxy'}) entry page
    res                         <- runStep (deleteCard {hashFunc, proxy: proxy''} (unwrap entry).cardReference card) (WidgetState (spinnerOverlay "Delete cards" White) page)
    pure $ discardResult res
  ) (pure $ ProxyResponse proxy unit) list