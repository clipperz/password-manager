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
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Argonaut.Core (stringify)
import Data.Array (filter, foldM, length, snoc)
import Data.Codec.Argonaut (encode)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), isRight)
import Data.FoldableWithIndex (foldWithIndexM)
import Data.Function (flip, (#), ($))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer, hex)
import Data.HeytingAlgebra (not)
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (AppState, CardsCache, InvalidStateError(..), ProxyResponse(..), discardResult)
import DataModel.CardVersions.Card (Card, CardVersion(..), fromCard)
import DataModel.CardVersions.Card as DataModel.CardVersions.Card
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion)
import DataModel.Credentials (emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..), Index(..), addToIndex)
import DataModel.UserVersions.User (IndexReference(..), UserInfo(..))
import DataModel.WidgetState (CardManagerState, CardViewState(..), ImportStep(..), LoginType(..), Page(..), UserAreaPage(..), UserAreaState, WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Card (addTag)
import Functions.Communication.Backend (ConnectionState, genericRequest)
import Functions.Communication.Blobs (deleteBlob, getBlob)
import Functions.Communication.Cards (deleteCard, getCard, postCard)
import Functions.Communication.Users (computeRemoteUserCard, deleteUserCard, deleteUserInfo, updateUserPreferences)
import Functions.Export (BlobsList, appendCardsDataInPlace, getBasicHTML, prepareUnencryptedExport, prepareHTMLBlob)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, noOperation, handleOperationResult, runStep)
import Functions.Import (ImportVersion(..), decodeImport, parseImport, readFile)
import Functions.Index (updateIndex)
import Functions.Pin (deleteCredentials, makeKey, saveCredentials)
import Functions.State (resetState)
import Functions.Time (formatDateTimeToDate, getCurrentDateTime)
import Functions.Timer (activateTimer, stopTimer)
import Functions.User (changeUserPassword)
import Record (merge)
import Views.ExportView (ExportEvent(..))
import Views.LoginFormView (emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SetPinView (PinEvent(..))
import Views.UserAreaView (UserAreaEvent(..), userAreaInitialState)
import Web.DownloadJs (download)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem)

handleUserAreaEvent :: UserAreaEvent -> CardManagerState -> UserAreaState -> AppState -> Fragment.FragmentState -> Widget HTML OperationState


handleUserAreaEvent userAreaEvent cardManagerState userAreaState state@{proxy, srpConf, hash: hashFunc, cardsCache, username: Just username, password: Just password, index: Just index, userInfo: Just userInfo@(UserInfo {indexReference: IndexReference { reference: indexRef}, userPreferences}), userInfoReferences: Just userInfoReferences, c: Just c, p: Just p, pinEncryptedPassword, donationLevel: Just donationLevel} _ = do
  let defaultPage = { index
                    , credentials:      {username, password}
                    , pinExists:        isJust pinEncryptedPassword
                    , userPreferences
                    , userAreaState
                    , cardManagerState
                    , donationLevel
                    }

  case userAreaEvent of
    (CloseUserAreaEvent) -> 
      noOperation (Tuple 
                  state
                  (WidgetState
                    hiddenOverlayInfo
                    (Main defaultPage {userAreaState = userAreaState {showUserArea = false, userAreaOpenPage = None}})
                  )
                )
    
    (UpdateUserPreferencesEvent newUserPreferences) -> 
      let page = Main defaultPage { userPreferences = newUserPreferences }
      in do
        ProxyResponse proxy' stateUpdateInfo <- runStep (updateUserPreferences state newUserPreferences) (WidgetState (spinnerOverlay "Update user preferences" White) page)
        
        liftEffect $ stopTimer
        case (unwrap newUserPreferences).automaticLock of
          Left  _ -> pure unit
          Right n -> liftEffect $ activateTimer n

        pure (Tuple 
          (merge stateUpdateInfo state {proxy = proxy'})
          (WidgetState hiddenOverlayInfo page)
        )
            
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White   

    (ChangePasswordEvent newPassword) ->
      let page      = Main defaultPage { credentials = {username, password: newPassword} }
          errorPage = Main defaultPage
      in do
        ProxyResponse proxy' userUpdateInfo <- runStep (changeUserPassword state newPassword) (WidgetState (spinnerOverlay "Update password" White) page)
        pure (Tuple 
          (state {proxy = proxy', c = Just userUpdateInfo.c, p = Just userUpdateInfo.p, s = Just userUpdateInfo.s, masterKey = Just userUpdateInfo.masterKey, password = Just newPassword})
          (WidgetState hiddenOverlayInfo page)
        )

      # runExceptT
      >>= handleOperationResult state errorPage true White
    
    (SetPinEvent pinAction) ->
      let page = Main defaultPage { pinExists = case pinAction of
                                                  Reset    -> false
                                                  SetPin _ -> true
                                  }
      in do
        storage               <- liftEffect $ window >>= localStorage
        pinEncryptedPassword' <- runStep (case pinAction of
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
                (state {pinEncryptedPassword = pinEncryptedPassword'})
                (WidgetState
                  hiddenOverlayInfo
                  page
                )
              )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White

    (DeleteAccountEvent) ->
      let page = Main defaultPage
      in do
        let connectionState = {proxy, hashFunc, srpConf, c, p}
        ProxyResponse proxy'    _ <-          deleteCardsSteps connectionState                   cardsCache       index                                                                     page
        ProxyResponse proxy''   _ <- runStep (deleteBlob       connectionState{proxy = proxy'  } indexRef (unwrap index).identifier) (WidgetState (spinnerOverlay "Delete Index"     White) page)
        ProxyResponse proxy'''  _ <- runStep (deleteUserInfo   connectionState{proxy = proxy'' } userInfo userInfoReferences       ) (WidgetState (spinnerOverlay "Delete User Info" White) page)
        ProxyResponse proxy'''' _ <- runStep (deleteUserCard   connectionState{proxy = proxy'''} c                                 ) (WidgetState (spinnerOverlay "Delete User Card" White) page)
        _                         <- liftEffect $ window >>= localStorage >>= deleteCredentials
        pure $ Tuple 
                (resetState state {proxy = proxy''''})
                (WidgetState
                  hiddenOverlayInfo
                  (Login emptyLoginFormData { credentials = emptyCredentials
                                            , loginType   = CredentialLogin
                                            }
                  )
                ) 
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true White
   
    (ImportCardsEvent importState) ->
      let page = Main defaultPage { userAreaState = userAreaState {importState = importState} }
      in case importState.step of
        Upload    ->
          do
            result    <- runStep  (case importState.content of
                                    Left  file   -> readFile file >>= parseImport
                                    Right string -> flip decodeImport string <$> [Epsilon CardVersion_1, Delta] 
                                                    # (foldM  (\finalResult singleResult -> do
                                                                if (isRight finalResult) 
                                                                then pure $ finalResult
                                                                else do
                                                                  res <- runExceptT singleResult
                                                                  if (isRight res)
                                                                  then pure $ res
                                                                  else pure $ finalResult
                                                              )
                                                              (Left $ ImportError "Invalid input: unable to decode data")
                                                      )
                                                    # ExceptT
                                  ) (WidgetState (spinnerOverlay "Parse Data" White) page)

            currentDate <- runStep ((((<>) "Import_") <<< formatDateTimeToDate) <$> (liftEffect getCurrentDateTime)) (WidgetState (spinnerOverlay "Get current date" White) page)

            pure $ Tuple 
                    state $
                    WidgetState
                      hiddenOverlayInfo $
                      Main defaultPage  { userAreaState = userAreaState 
                                                            { importState = importState
                                                                              { content = Right $ stringify $ encode (CA.array currentCardCodecVersion) $ fromCard <$> result
                                                                              , step = Selection, tag = Tuple true currentDate, selection = result <#> (\card@(DataModel.CardVersions.Card.Card r) -> Tuple (not r.archived) card)
                                                                              }
                                                            }
                                        }

          # runExceptT
          >>= handleOperationResult state page true White
      
        Selection ->
          noOperation $ Tuple 
                        state $
                        WidgetState
                          hiddenOverlayInfo $
                          Main defaultPage { userAreaState = userAreaState {importState = importState {step = Confirm}} }
        
        Confirm   ->
          do
            let connectionState = {proxy, hashFunc, srpConf, c, p}
            let cardToImport                                  = filter fst importState.selection <#> snd # (if fst importState.tag
                                                                                                            then (map $ addTag (snd importState.tag))
                                                                                                            else identity
                                                                                                          )
            let nToImport                                     = length cardToImport
            ProxyResponse proxy' (Tuple cardsCache' entries) <- foldWithIndexM (\i (ProxyResponse proxy' (Tuple cardsCache' entries)) card -> do
              ProxyResponse proxy'' (Tuple cardsCache'' newCardEntry) <- runStep (postCard connectionState{proxy = proxy'} cardsCache' card) (WidgetState (spinnerOverlay ("Import card " <> show i <> " of " <> show nToImport) White) page)
              pure $ ProxyResponse proxy'' (Tuple cardsCache'' (snoc entries newCardEntry))
            ) (ProxyResponse proxy (Tuple cardsCache [])) cardToImport
            updatedIndex                                     <- runStep (foldM (flip addToIndex) index entries # liftAff)   (WidgetState (spinnerOverlay "Update index" White) page)
            ProxyResponse proxy'' stateUpdateInfo            <- runStep (updateIndex (state {proxy = proxy'}) updatedIndex) (WidgetState (spinnerOverlay "Update index" White) page)

            pure (Tuple 
              (merge stateUpdateInfo state {proxy = proxy'', cardsCache = cardsCache', index = Just updatedIndex}
              )
              (WidgetState
                hiddenOverlayInfo
                (Main defaultPage { index            = updatedIndex
                                  , userAreaState    = userAreaInitialState
                                  , cardManagerState = cardManagerState {cardViewState = NoCard, highlightedEntry = Nothing}
                                  }
                )
              )
          )
      
          # runExceptT
          >>= handleOperationResult state page true White  

    (ExportEvent OfflineCopy) ->
      let page = Main defaultPage
      in do
        let connectionState         =         {proxy, hashFunc, srpConf, c, p}
        let references              =         userInfoReferences.reference : indexRef : ((\(CardEntry {cardReference: CardReference {reference}}) -> reference) <$> (unwrap index).entries)

        ProxyResponse proxy' blobs <-          downloadBlobsSteps references connectionState page
        remoteUserCard             <- runStep (computeRemoteUserCard state)                                                                                  (WidgetState (spinnerOverlay "Compute user card" White) page)
        ProxyResponse proxy'' doc  <- runStep (getBasicHTML connectionState{proxy = proxy'})                                                                 (WidgetState (spinnerOverlay "Download html"     White) page)
        documentToDownload         <- runStep (appendCardsDataInPlace doc blobs remoteUserCard >>= (liftEffect <<< prepareHTMLBlob))                         (WidgetState (spinnerOverlay "Create document"   White) page)
        date                       <- runStep (liftEffect $ formatDateTimeToDate <$> getCurrentDateTime)                                                     (WidgetState (spinnerOverlay ""                  White) page)
        _                          <- runStep (liftEffect $ download documentToDownload (date <> "_Clipperz_Offline" <> ".html") "application/octet-stream") (WidgetState (spinnerOverlay "Download document" White) page)
        pure $ Tuple state {proxy = proxy''} (WidgetState hiddenOverlayInfo page)
      
      # runExceptT
      >>= handleOperationResult state page true White

    (ExportEvent UnencryptedCopy) ->
      let page = Main defaultPage
      in do
        let connectionState = {proxy, hashFunc, srpConf, c, p}
        ProxyResponse proxy' (Tuple cardsCache' cardList) <-                       downloadCardsSteps (unwrap index).entries cardsCache connectionState page
        doc                                               <- runStep (liftEffect $ prepareUnencryptedExport cardList)                                             (WidgetState (spinnerOverlay "Create document"   White) page)
        date                                              <- runStep (liftEffect $ formatDateTimeToDate <$> getCurrentDateTime)                                   (WidgetState (spinnerOverlay ""                  White) page)
        _                                                 <- runStep (liftEffect $ download doc (date <> "_Clipperz_Export_" <> username <> ".html") "text/html") (WidgetState (spinnerOverlay "Download document" White) page)
                                  
        pure $ Tuple state{proxy = proxy', cardsCache = cardsCache'} (WidgetState hiddenOverlayInfo page)
      # runExceptT
      >>= handleOperationResult state page true White

    (LockEvent) ->
      let page = Main defaultPage
      in
        logoutSteps state "Lock" page
        # runExceptT
        >>= handleOperationResult state defaultErrorPage true White
  
    (LogoutEvent) ->
      let page = Main defaultPage
      in
        logoutSteps (state {username = Nothing}) "Logout" page
        # runExceptT
        >>= handleOperationResult state defaultErrorPage true White

handleUserAreaEvent _ _ _ state _ = do
  throwError $ InvalidStateError (CorruptedState "userAreaEvent")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true White

-- ===================================================================================================

downloadCardsSteps :: List CardEntry -> CardsCache -> ConnectionState -> Page -> ExceptT AppError (Widget HTML) (ProxyResponse (Tuple CardsCache (List Card)))
downloadCardsSteps cardEntryList cardsCache connectionState page = 
  foldWithIndexM (\i (ProxyResponse proxy' (Tuple cardsCache' cards)) cardEntry -> do
    ProxyResponse proxy'' (Tuple cardsCache'' card) <- runStep (getCard connectionState{proxy = proxy'} cardsCache' cardEntry) (WidgetState (spinnerOverlay ("Download card " <> show i <> " of " <> show nToDownload) White) page)
    pure $ ProxyResponse proxy'' (Tuple cardsCache'' (List.snoc cards card))
  ) (ProxyResponse connectionState.proxy (Tuple cardsCache Nil)) cardEntryList
  where
    nToDownload = List.length cardEntryList

downloadBlobsSteps :: List HexString -> ConnectionState -> Page -> ExceptT AppError (Widget HTML) (ProxyResponse BlobsList)
downloadBlobsSteps referenceList connectionState page = 
  foldWithIndexM (\i (ProxyResponse proxy' blobs) reference -> do
    ProxyResponse proxy'' arrayBuffer <- runStep (getBlob connectionState{proxy = proxy'} reference) (WidgetState (spinnerOverlay ("Download blob " <> show i <> " of " <> show nToDownload) White) page)
    pure $ ProxyResponse proxy'' (List.snoc blobs (Tuple reference (fromArrayBuffer arrayBuffer)))
  ) (ProxyResponse connectionState.proxy Nil) referenceList
  where
    nToDownload = List.length referenceList

logoutSteps :: AppState -> String -> Page -> ExceptT AppError (Widget HTML) OperationState
logoutSteps state@{username, hash: hashFunc, proxy, srpConf} message page =
  do
    let connectionState = {proxy, hashFunc, srpConf, c: hex "", p: hex ""}
    passphrase <- runStep (do 
                            _   <- genericRequest connectionState "logout" POST Nothing RF.ignore
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

deleteCardsSteps :: ConnectionState -> CardsCache -> Index -> Page -> ExceptT AppError (Widget HTML) (ProxyResponse Unit)
deleteCardsSteps connectionState cardsCache (Index {entries}) page =
  foldWithIndexM (\i (ProxyResponse proxy' _) cardEntry -> do
    discardResult <$> runStep (deleteCard connectionState{proxy = proxy'} cardsCache (unwrap cardEntry).cardReference) (WidgetState (spinnerOverlay ("Delete card " <> show i <> " of " <> show nToDelete) White) page)
  ) (ProxyResponse connectionState.proxy unit) entries

  where
    nToDelete = List.length entries