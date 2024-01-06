module Functions.Handler.UserAreaEventHandler
  ( handleUserAreaEvent
  )
  where

import Affjax.ResponseFormat as RF
import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt (map, ($>), (<#>), (<$>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Category (identity, (<<<))
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT, throwError)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (filter, foldr, length, snoc)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Function ((#), ($))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer, hex)
import Data.HeytingAlgebra (not)
import Data.List (List(..), foldl, (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.Card (Card)
import DataModel.Card as DataModel.Card
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.Index (CardEntry(..), CardReference(..), Index(..), addToIndex)
import DataModel.AppState (InvalidStateError(..), ProxyResponse(..), AppState, discardResult)
import DataModel.User (IndexReference(..), UserInfoReferences(..), UserPreferencesReference(..))
import Effect.Class (liftEffect)
import Functions.Card (addTag)
import Functions.Communication.Blobs (deleteBlobWithReference, getBlob)
import Functions.Communication.Cards (deleteCard, getCard, postCard)
import Functions.Communication.Backend (ConnectionState, manageGenericRequest)
import Functions.Communication.Users (computeRemoteUserCard, deleteUserCard, deleteUserInfo, updateIndex, updateUserPreferences)
import Functions.Export (BlobsList, appendCardsDataInPlace, getBasicHTML, prepareCardsForUnencryptedExport, prepareHTMLBlob)
import Functions.Handler.CardManagerEventHandler (getCardSteps)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, doNothing, handleOperationResult, runStep)
import Functions.Import (decodeHTML, decodeImport, parseHTMLImport, readFile)
import Functions.Pin (deleteCredentials, makeKey, saveCredentials)
import Functions.State (resetState)
import Functions.Time (formatDateTimeToDate, getCurrentDateTime)
import Functions.Timer (activateTimer, stopTimer)
import Functions.User (changeUserPassword)
import Views.AppView (Page(..), WidgetState(..))
import Views.CardsManagerView (CardManagerState, CardViewState(..))
import Views.ExportView (ExportEvent(..))
import Views.ImportView (ImportStep(..))
import Views.LoginFormView (LoginType(..), emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SetPinView (PinEvent(..))
import Views.UserAreaView (UserAreaEvent(..), UserAreaPage(..), UserAreaState, userAreaInitialState)
import Web.DownloadJs (download)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

handleUserAreaEvent :: UserAreaEvent -> CardManagerState -> UserAreaState -> AppState -> Fragment.FragmentState -> Widget HTML OperationState


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
    pure $ Tuple 
            (resetState state {proxy = proxy'''''})
            (WidgetState
              hiddenOverlayInfo
              (Login emptyLoginFormData { credentials = emptyCredentials
                                        , loginType   = CredentialLogin
                                        }
              )
            ) 
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


handleUserAreaEvent (ImportCardsEvent importState) cardManagerState userAreaState state@{proxy, index: Just index, username: Just username, password: Just password, userPreferences: Just userPreferences, pinEncryptedPassword} _ = 
  case importState.step of
    Upload    ->
      do
        toDecode    <- runStep (case importState.content of
                                 Left  file   -> do
                                   fileContent     <- readFile file
                                   codedCardData   <- except $ parseHTMLImport fileContent
                                   (pure $ decodeHTML codedCardData) <#> Left
                                 Right string -> (except $ lmap (ProtocolError <<< DecodeError) $ jsonParser string) <#> Right
                               ) (WidgetState (spinnerOverlay "Parse File" White) page)
        
        result      <- runStep (case toDecode of
                                 Left  htmlData -> ExceptT $ liftEffect $ decodeImport htmlData
                                 Right jsonData -> except  $ lmap (ProtocolError <<< DecodeError <<< show) $ decodeJson jsonData
                               ) (WidgetState (spinnerOverlay "Decode Data" White) page)

        currentDate <- runStep ((((<>) "Import_") <<< formatDateTimeToDate) <$> (liftEffect getCurrentDateTime)) (WidgetState (spinnerOverlay "Get current date" White) page)

        pure $ Tuple 
                state $
                WidgetState
                  hiddenOverlayInfo $
                  Main { index
                       , credentials: {username, password}
                       , pinExists: isJust pinEncryptedPassword
                       , cardManagerState
                       , userAreaState : userAreaState {importState = importState {content = Right $ stringify $ encodeJson result, step = Selection, tag = Tuple true currentDate, selection = result <#> (\c@(DataModel.Card.Card r) -> Tuple (not r.archived) c)}}
                       , userPreferences
                       }

      # runExceptT
      >>= handleOperationResult state page true White
    
    Selection ->
      doNothing $ Tuple 
                    state $
                    WidgetState
                      hiddenOverlayInfo $
                      Main { index
                           , credentials: {username, password}
                           , pinExists: isJust pinEncryptedPassword
                           , cardManagerState
                           , userAreaState : userAreaState {importState = importState {step = Confirm}}
                           , userPreferences
                           }
    
    Confirm   ->
      do
        let cardToImport                      = filter fst importState.selection <#> snd # (if fst importState.tag
                                                                                            then (map $ addTag (snd importState.tag))
                                                                                            else identity
                                                                                           )
        let nToImport                         = length cardToImport
        ProxyResponse proxy' entries          <- foldWithIndexM (\i (ProxyResponse proxy' entries) card -> do
          ProxyResponse proxy'' newCardEntry  <- runStep (postCard state {proxy = proxy'} card) (WidgetState (spinnerOverlay ("Import card " <> show i <> " of " <> show nToImport) White) page)
          pure $ ProxyResponse proxy'' (snoc entries newCardEntry)
        ) (ProxyResponse proxy []) cardToImport
        let updatedIndex                       = foldr addToIndex index entries        
        ProxyResponse proxy'' stateUpdateInfo <- runStep (updateIndex (state {proxy = proxy'}) updatedIndex) (WidgetState (spinnerOverlay "Update index" White) page)

        pure (Tuple 
          (state { proxy = proxy''
                 , index = Just updatedIndex
                 , userInfoReferences = Just stateUpdateInfo.newUserInfoReferences
                 , masterKey = Just stateUpdateInfo.newMasterKey
                 }
          )
          (WidgetState
            hiddenOverlayInfo
            (Main { index:            updatedIndex
                  , credentials:      {username, password}
                  , pinExists: isJust pinEncryptedPassword
                  , userPreferences
                  , userAreaState:    userAreaInitialState
                  , cardManagerState: cardManagerState {cardViewState = NoCard, selectedEntry = Nothing}
                  }
            )
          )
        )
      
      
      # runExceptT
      >>= handleOperationResult state page true White  
  where
    page = Main { index
                , credentials: {username, password}
                , pinExists: isJust pinEncryptedPassword
                , cardManagerState
                , userAreaState : userAreaState {importState = importState}
                , userPreferences
                }


handleUserAreaEvent (ExportEvent OfflineCopy) cardManagerState userAreaState state@{proxy, hash: hashFunc, index: Just index@(Index cardEntryList), username: Just username, password: Just password, userPreferences: Just userPreferences, userInfoReferences: Just (UserInfoReferences { indexReference: IndexReference { reference: indexRef }, preferencesReference: UserPreferencesReference { reference: prefRef}}), pinEncryptedPassword} _ =
  do
    let references = prefRef : indexRef : ((\(CardEntry {cardReference: CardReference {reference}}) -> reference) <$> cardEntryList)
    
    ProxyResponse proxy' blobs <- downloadBlobsSteps references {hashFunc, proxy} page
    remoteUserCard             <- runStep (computeRemoteUserCard state)                                                                                  (WidgetState (spinnerOverlay "Compute user card" White) page)
    ProxyResponse proxy'' doc  <- runStep (getBasicHTML {hashFunc, proxy: proxy'})                                                                       (WidgetState (spinnerOverlay "Download html"     White) page)
    documentToDownload         <- runStep (appendCardsDataInPlace doc blobs remoteUserCard >>= (liftEffect <<< prepareHTMLBlob))                                                              (WidgetState (spinnerOverlay "Create document"   White) page)
    date                       <- runStep (liftEffect $ formatDateTimeToDate <$> getCurrentDateTime)                                                     (WidgetState (spinnerOverlay ""                  White) page)
    _                          <- runStep (liftEffect $ download documentToDownload (date <> "_Clipperz_Offline" <> ".html") "application/octet-stream") (WidgetState (spinnerOverlay "Download document" White) page)
    pure $ Tuple state {proxy = proxy''} (WidgetState hiddenOverlayInfo page)
  # runExceptT
  >>= handleOperationResult state page true White
  
  where
      page = Main { index
                  , credentials:      {username, password}
                  , pinExists:        isJust pinEncryptedPassword
                  , userPreferences
                  , userAreaState
                  , cardManagerState
                  }

handleUserAreaEvent (ExportEvent UnencryptedCopy) cardManagerState userAreaState state@{index: Just index@(Index cardEntryList), username: Just username, password: Just password, userPreferences: Just userPreferences, pinEncryptedPassword} _ =
  do
    Tuple state' cardList <- downloadCardsSteps cardEntryList state page
    doc                   <- runStep (liftEffect $ prepareCardsForUnencryptedExport cardList)                                                    (WidgetState (spinnerOverlay "Create document"   White) page)
    date                  <- runStep (liftEffect $ formatDateTimeToDate <$> getCurrentDateTime)                                                  (WidgetState (spinnerOverlay ""                  White) page)
    _                     <- runStep (liftEffect $ download doc (date <> "_Clipperz_Export_" <> username <> ".html") "application/octet-stream") (WidgetState (spinnerOverlay "Download document" White) page)
  
    pure $ Tuple state' (WidgetState hiddenOverlayInfo page)
  # runExceptT
  >>= handleOperationResult state page true White
  
  where
      page = Main { index
                  , credentials:      {username, password}
                  , pinExists:        isJust pinEncryptedPassword
                  , userPreferences
                  , userAreaState
                  , cardManagerState
                  }

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

downloadCardsSteps :: List CardEntry -> AppState -> Page -> ExceptT AppError (Widget HTML) (Tuple AppState (List Card))
downloadCardsSteps cardEntryList state page = 
  foldWithIndexM (\i (Tuple state' cards) cardEntry -> do
    Tuple state'' card <- runStep (getCard state' cardEntry) (WidgetState (spinnerOverlay ("Download card " <> show i <> " of " <> show nToDownload) White) page)
    pure $ Tuple state'' (List.snoc cards card)
  ) (Tuple state Nil) cardEntryList
  where
    nToDownload = List.length cardEntryList

downloadBlobsSteps :: List HexString -> ConnectionState -> Page -> ExceptT AppError (Widget HTML) (ProxyResponse BlobsList)
downloadBlobsSteps referenceList {proxy, hashFunc} page = 
  foldWithIndexM (\i (ProxyResponse proxy' blobs) reference -> do
    ProxyResponse proxy'' arrayBuffer <- runStep (getBlob {proxy: proxy', hashFunc} reference) (WidgetState (spinnerOverlay ("Download blob " <> show i <> " of " <> show nToDownload) White) page)
    pure $ ProxyResponse proxy'' (List.snoc blobs (Tuple reference (fromArrayBuffer arrayBuffer)))
  ) (ProxyResponse proxy Nil) referenceList
  where
    nToDownload = List.length referenceList

logoutSteps :: AppState -> String -> Page -> ExceptT AppError (Widget HTML) OperationState
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

deleteCardsSteps :: AppState -> Index -> Page -> ExceptT AppError (Widget HTML) (ProxyResponse Unit)
deleteCardsSteps state@{hash: hashFunc, proxy} (Index list) page =
  foldl (\proxyResponse entry -> do
    ProxyResponse proxy' _      <- proxyResponse
    Tuple {proxy: proxy''} card <- getCardSteps (state {proxy = proxy'}) entry page
    res                         <- runStep (deleteCard {hashFunc, proxy: proxy''} (unwrap entry).cardReference card) (WidgetState (spinnerOverlay "Delete cards" White) page)
    pure $ discardResult res
  ) (pure $ ProxyResponse proxy unit) list