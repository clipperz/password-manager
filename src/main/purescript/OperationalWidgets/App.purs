module OperationalWidgets.App ( app ) where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<#>), (<|>))
import Control.Alternative ((*>), (<*))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Except (except, throwError)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.CommutativeRing ((+))
import Data.Either (Either(..), either, note)
import Data.Function ((#), ($))
import Data.Functor ((<$))
import Data.HexString (toArrayBuffer)
import Data.Int (fromString, toNumber)
import Data.Map (delete, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Ord ((<))
import Data.Show (show)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Card as DataModel.Card
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.Index (CardEntry(..), addToIndex, emptyIndex, reference, removeFromIndex)
import DataModel.Password (standardPasswordGeneratorSettings)
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.Card (appendToTitle, archiveCard, getCardContent, restoreCard)
import Functions.Communication.Blobs (getStatelessBlob)
import Functions.Communication.Cards (deleteCard, postCard)
import Functions.Communication.Login (PrepareLoginResult, loginStep1, loginStep2, prepareLogin)
import Functions.Communication.Signup (signupUser)
import Functions.Communication.Users (getStatelessIndex, getStatelessUserPreferences, updateIndex)
import Functions.Pin (decryptPassphraseWithPin, deleteCredentials, makeKey)
import Functions.SRP (checkM2)
import Functions.Timer (activateTimer)
import Record (merge)
import Unsafe.Coerce (unsafeCoerce)
import Views.AppView (Page(..), PageEvent(..), UserAreaEvent(..), WidgetState(..), appView, emptyMainPageWidgetState, loadingMainPage)
import Views.CardsManagerView (CardFormInput(..), CardManagerEvent(..), CardViewState(..), CardManagerState, cardManagerInitialState)
import Views.LoginFormView (LoginPageEvent(..), LoginType(..), LoginFormData, emptyLoginFormData)
import Views.OverlayView (OverlayStatus(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SignupFormView (SignupPageEvent(..), emptyDataForm, getSignupDataFromCredentials)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

-- =========================================================
--  manage application effects (state, local storage, api)
-- =========================================================

getLoginFormData :: StatelessAppState -> LoginFormData
getLoginFormData {username: Just username, pinEncryptedPassword: Just _} = emptyLoginFormData { credentials = {username, password: ""}, loginType = PinLogin }
getLoginFormData _ = emptyLoginFormData

app :: forall a. StatelessAppState -> Fragment.FragmentState -> Widget HTML a
app appState fragmentState = case fragmentState of
    Fragment.Login cred   -> appWithInitialOperation appState (LoginPageEvent $ LoginEvent cred)
    Fragment.Registration -> appLoop          (Tuple appState (WidgetState hiddenOverlayInfo (Signup  emptyDataForm)))
    _                     -> appLoop          (Tuple appState (WidgetState hiddenOverlayInfo (Login $ getLoginFormData appState)))
  
  where
    appWithInitialOperation :: StatelessAppState -> PageEvent -> Widget HTML a
    appWithInitialOperation state event = do
      appLoop =<< executeOperation event state fragmentState

    appLoop :: (Tuple StatelessAppState WidgetState) -> Widget HTML a
    appLoop (Tuple state widgetState) = do

      resultEvent <- appView widgetState
        
      appLoop =<< executeOperation resultEvent state fragmentState

type OperationState = Tuple StatelessAppState WidgetState

executeOperation :: PageEvent -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState
executeOperation (SignupPageEvent          event)       = handleSignupPageEvent  event
executeOperation (LoginPageEvent           event)       = handleLoginPageEvent   event
executeOperation (MainPageCardManagerEvent event state) = handleCardManagerEvent event state
executeOperation (MainPageUserAreaEvent    event)       = handleUserAreaEvent    event


-- ============ HANDLE SIGNUP PAGE EVENTS ============

handleSignupPageEvent :: SignupPageEvent -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState 


handleSignupPageEvent (SignupEvent cred) state@{proxy, hash, srpConf} fragmentState = 
  do
    ProxyResponse newProxy signupResult <- runStep (signupUser proxy hash srpConf cred) (WidgetState (spinnerOverlay "registering") initialPage)
    res                                 <- loginSteps cred (state {proxy = newProxy}) fragmentState initialPage signupResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state initialPage

  where
    initialPage = Signup $ getSignupDataFromCredentials cred


handleSignupPageEvent (GoToLoginEvent cred) state _ = doNothing $ Tuple state (WidgetState hiddenOverlayInfo (Login (getLoginFormData state) {credentials = cred}))

-- ============ HANDLE LOGIN PAGE EVENTS ============

handleLoginPageEvent :: LoginPageEvent -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState 


handleLoginPageEvent (LoginEvent cred) state@{proxy, srpConf} fragmentState =
  do
    ProxyResponse proxy' prepareLoginResult <- runStep (prepareLogin proxy srpConf cred) (WidgetState (spinnerOverlay "Prepare login") initialPage)
    res                                     <- loginSteps cred (state {proxy = proxy'}) fragmentState initialPage prepareLoginResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state initialPage

  where 
    initialPage = (Login emptyLoginFormData {credentials = cred})


handleLoginPageEvent (LoginPinEvent pin) state@{proxy, hash, srpConf, username, pinEncryptedPassword} fragmentState = do
  do
    cred                                    <- runStep (decryptPassphraseWithPin hash pin username pinEncryptedPassword) (WidgetState (spinnerOverlay "Decrypt with PIN") initialPage)
    ProxyResponse proxy' prepareLoginResult <- runStep (prepareLogin proxy srpConf cred)                                 (WidgetState (spinnerOverlay "Prepare login"   ) initialPage)
    res                                     <- loginSteps cred (state {proxy = proxy'}) fragmentState initialPage prepareLoginResult
    pure res
  
  # runExceptT
  >>= handlePinResult state initialPage
  >>= (\(Tuple page either) -> handleOperationResult state page either)

  where
    initialPage = Login emptyLoginFormData {pin = pin, loginType = PinLogin}


handleLoginPageEvent (GoToSignupEvent cred) state _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Signup emptyDataForm {username = cred.username, password = cred.password})))


handleLoginPageEvent (GoToCredentialLoginEvent username) state _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Login emptyLoginFormData {credentials = {username, password: ""}})))

-- ============ HANDLE CARD MANAGER EVENTS ============

handleCardManagerEvent :: CardManagerEvent -> CardManagerState -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState


handleCardManagerEvent OpenUserAreaEvent cardManagerState state@{index, userPreferences} _ = doNothing (Tuple state (WidgetState hiddenOverlayInfo (Main { index: fromMaybe emptyIndex index, showUserArea: true, cardManagerState, userPasswordGeneratorSettings: maybe standardPasswordGeneratorSettings ((\up -> (unwrap up).passwordGeneratorSettings)) userPreferences })))


handleCardManagerEvent (OpenCardViewEvent cardEntry) cardManagerState state@{index, userPreferences} _ = 
  do
    index'                        <- except $ note (InvalidStateError $ CorruptedState "index not found")             index
    userPasswordGeneratorSettings <- except $ note (InvalidStateError $ CorruptedState "user preferences not found") (userPreferences <#> (\up -> (unwrap up).passwordGeneratorSettings))

    Tuple state' card <- getCardSteps state cardEntry (Main { index: index', showUserArea: false, cardManagerState, userPasswordGeneratorSettings: standardPasswordGeneratorSettings})
    pure (Tuple
            state'
            (WidgetState
              hiddenOverlayInfo
              (Main { index: index'
                    , showUserArea: false
                    , cardManagerState: cardManagerState {selectedEntry = (Just cardEntry), cardViewState = Card card cardEntry}
                    , userPasswordGeneratorSettings
                    }
              )
            )
         ) 

  # runExceptT
  >>= handleOperationResult state defaultErrorPage


handleCardManagerEvent (AddCardEvent card) cardManagerState state@{index} _ = 
  do
  do
    index' <- except $ note (InvalidStateError $ CorruptedState "index not found") index

    res    <- addCardSteps cardManagerState state card (loadingMainPage index' cardManagerState) "Add card"
    pure res

  # runExceptT
  >>= handleOperationResult state defaultErrorPage
  --   index' <- except $ note (InvalidStateError $ CorruptedState "index not found") index
    
  --   res    <- addCardStepsWithStateT card (loadingMainPage index' (CardForm $ NewCard $ Just card)) "Add card"
  --   pure res

  -- # runExceptT
  -- # flip runStateT state
  -- >>= handleOperationResultWithStateT defaultErrorPage


handleCardManagerEvent (CloneCardEvent cardEntry) cardManagerState state@{index} _ = 
  do
    index'            <- except $ note (InvalidStateError $ CorruptedState "index not found") index

    Tuple state' card <- getCardSteps state cardEntry (loadingMainPage index' cardManagerState)
    let cloneCard      = appendToTitle " - copy" <<< (\(DataModel.Card.Card card') -> DataModel.Card.Card card' {archived = false}) $ card
    res               <- addCardSteps cardManagerState state' cloneCard (loadingMainPage index' cardManagerState) "Clone card"
    pure res

  # runExceptT
  >>= handleOperationResult state defaultErrorPage


handleCardManagerEvent (DeleteCardEvent cardEntry) cardManagerState state@{hash: hashFunc, index, userPreferences} _ =
  do
    index'                                <- except $ note (InvalidStateError $ CorruptedState "index not found")             index
    userPasswordGeneratorSettings         <- except $ note (InvalidStateError $ CorruptedState "user preferences not found") (userPreferences <#> (\up -> (unwrap up).passwordGeneratorSettings))

    Tuple state'@{proxy} card             <- getCardSteps state cardEntry (loadingMainPage index' cardManagerState)
    ProxyResponse proxy' _                <- runStep (deleteCard {proxy, hashFunc} (unwrap cardEntry).cardReference card) (WidgetState (spinnerOverlay "Delete card")  (loadingMainPage index' cardManagerState))
    let updatedIndex                       = removeFromIndex cardEntry index'
    ProxyResponse proxy'' stateUpdateInfo <- runStep (updateIndex (state' {proxy = proxy'}) updatedIndex)                 (WidgetState (spinnerOverlay "Update index") (loadingMainPage index' cardManagerState))

    pure (Tuple 
            (state' {proxy = proxy'', index = Just updatedIndex, userInfoReferences = Just stateUpdateInfo.newUserInfoReferences, masterKey = Just stateUpdateInfo.newMasterKey})
            (WidgetState
              hiddenOverlayInfo
              (Main { index: updatedIndex, showUserArea: false, cardManagerState: cardManagerState {cardViewState = NoCard, selectedEntry = Nothing}, userPasswordGeneratorSettings })
            )
         )

  # runExceptT
  >>= handleOperationResult state defaultErrorPage


handleCardManagerEvent (EditCardEvent oldCardEntry updatedCard) cardManagerState state@{index} _ =
  do
    index'                        <- except $ note (InvalidStateError $ CorruptedState "index not found") index
    
    Tuple state'          oldCard <- getCardSteps                   state  oldCardEntry                     (loadingMainPage index' cardManagerState)
    res                           <- editCardSteps cardManagerState state' oldCardEntry oldCard updatedCard (loadingMainPage index' cardManagerState)
    pure res
  
  # runExceptT
  >>= handleOperationResult state defaultErrorPage


handleCardManagerEvent (ArchiveCardEvent cardEntry) cardManagerState state@{index} _ =
  do
    index'                        <- except $ note (InvalidStateError $ CorruptedState "index not found")             index

    Tuple state'         card     <- getCardSteps                   state  cardEntry                  (loadingMainPage index' cardManagerState)
    let updatedCard                = archiveCard card
    res                           <- editCardSteps cardManagerState state' cardEntry card updatedCard (loadingMainPage index' cardManagerState)
    pure res

  # runExceptT
  >>= handleOperationResult state defaultErrorPage


handleCardManagerEvent (RestoreCardEvent cardEntry) cardManagerState state@{index} _ =
  do
    index'                        <- except $ note (InvalidStateError $ CorruptedState "index not found")             index

    Tuple state'         card     <- getCardSteps                   state  cardEntry                  (loadingMainPage index' cardManagerState)
    let updatedCard                = restoreCard card
    res                           <- editCardSteps cardManagerState state' cardEntry card updatedCard (loadingMainPage index' cardManagerState)
    pure res

  # runExceptT
  >>= handleOperationResult state defaultErrorPage  
  
-- ============ HANDLE USER AREA EVENTS ============

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

-- ===================================================================================================

doNothing :: OperationState -> Widget HTML OperationState 
doNothing operationState@(Tuple _ widgetState) = (pure operationState) <|> (unsafeCoerce unit <$ appView widgetState)

loginSteps :: Credentials -> StatelessAppState -> Fragment.FragmentState -> Page -> PrepareLoginResult -> ExceptT AppError (Widget HTML) OperationState
loginSteps cred state@{proxy, hash: hashFunc, srpConf} fragmentState page prepareLoginResult = do

  ProxyResponse proxy'   loginStep1Result <- runStep (loginStep1         proxy   hashFunc srpConf prepareLoginResult.c)                                       (WidgetState {status: Spinner, message: "SRP step 1"   } page)
  ProxyResponse proxy''  loginStep2Result <- runStep (loginStep2         proxy'  hashFunc srpConf prepareLoginResult.c prepareLoginResult.p loginStep1Result) (WidgetState {status: Spinner, message: "SRP step 2"   } page)
  _                                       <- runStep ((liftAff $ checkM2 srpConf loginStep1Result.aa loginStep2Result.m1 loginStep2Result.kk (toArrayBuffer loginStep2Result.m2)) >>= (\result -> 
                                                      if result
                                                      then pure         unit
                                                      else throwError $ ProtocolError (SRPError "Client M2 doesn't match with server M2")
                                                     ))                                                                                                       (WidgetState {status: Spinner, message: "Validate user"} page)

  let stateUpdate = { userInfoReferences: Just loginStep2Result.userInfoReferences 
                    , masterKey:          Just loginStep2Result.masterKey 
                    , username:           Just cred.username
                    , password:           Just cred.password
                    , s:                  Just loginStep1Result.s
                    , c:                  Just prepareLoginResult.c
                    , p:                  Just prepareLoginResult.p
                    }

  res                                     <- loadHomePageSteps (merge stateUpdate (state {proxy = proxy''})) fragmentState

  pure $ res

loadHomePageSteps :: StatelessAppState -> Fragment.FragmentState -> ExceptT AppError (Widget HTML) OperationState
loadHomePageSteps state@{hash: hashFunc, proxy, userInfoReferences: maybeUserInfoReferences} fragmentState = do
  userInfoReferences                    <- except $ note (InvalidStateError $ CorruptedState "UserInfoReferences is Nothing") maybeUserInfoReferences

  ProxyResponse proxy'  userPreferences <- runStep (getStatelessUserPreferences { proxy,            hashFunc } (unwrap userInfoReferences).preferencesReference) (WidgetState {status: Spinner, message: "Get user preferences"} $ Main emptyMainPageWidgetState)
  ProxyResponse proxy'' index           <- runStep (getStatelessIndex           { proxy: proxy',    hashFunc } (unwrap userInfoReferences).indexReference)       (WidgetState {status: Spinner, message: "Get index"}            $ Main emptyMainPageWidgetState)
  
  case (unwrap userPreferences).automaticLock of
    Right n -> liftEffect (activateTimer n)
    Left  _ -> pure unit

  let cardViewState = case fragmentState of
                        Fragment.AddCard card -> CardForm (NewCard $ Just card)
                        _                     -> NoCard

  pure $ Tuple (state {proxy = proxy'', index = Just index, userPreferences = Just userPreferences}) (WidgetState {status: Hidden, message: ""} (Main emptyMainPageWidgetState { index = index, cardManagerState = cardManagerInitialState { cardViewState = cardViewState } }))

getCardSteps :: StatelessAppState -> CardEntry -> Page -> ExceptT AppError (Widget HTML) (Tuple StatelessAppState DataModel.Card.Card)
getCardSteps state@{proxy, hash, cardsCache} cardEntry@(CardEntry entry) page = do
  let cardFromCache = lookup (reference cardEntry) cardsCache
  case cardFromCache of
    Just card -> pure $ Tuple state card
    Nothing   -> do
      ProxyResponse proxy' blob <- runStep (getStatelessBlob {proxy, hashFunc: hash} (reference cardEntry)) (WidgetState (spinnerOverlay "Get card")     page)  
      card                      <- runStep (getCardContent blob (entry.cardReference))                      (WidgetState (spinnerOverlay "Decrypt card") page)
      let updatedCardsCache = insert (reference cardEntry) card cardsCache
      pure $ Tuple
        (state { proxy      = proxy'
               , cardsCache = updatedCardsCache})
         card

addCardSteps :: CardManagerState -> StatelessAppState -> DataModel.Card.Card -> Page -> String -> ExceptT AppError (Widget HTML) OperationState
addCardSteps cardManagerState state@{index, userPreferences, cardsCache} newCard page message = do
  index'                                <- except $ note (InvalidStateError $ CorruptedState "index not found")             index
  userPasswordGeneratorSettings         <- except $ note (InvalidStateError $ CorruptedState "user preferences not found") (userPreferences <#> (\up -> (unwrap up).passwordGeneratorSettings))

  ProxyResponse proxy'  newCardEntry    <- runStep (postCard state newCard)                            (WidgetState (spinnerOverlay message)        page)
  let updatedIndex                       = addToIndex newCardEntry index'
  ProxyResponse proxy'' stateUpdateInfo <- runStep (updateIndex (state {proxy = proxy'}) updatedIndex) (WidgetState (spinnerOverlay "Update index") page)
  let updatedCardCache                   = insert (reference newCardEntry) newCard cardsCache

  pure (Tuple 
          (state { proxy = proxy''
                  , index = Just updatedIndex
                  , userInfoReferences = Just stateUpdateInfo.newUserInfoReferences
                  , masterKey = Just stateUpdateInfo.newMasterKey
                  , cardsCache = updatedCardCache
                  }
          )
          (WidgetState
            hiddenOverlayInfo
            (Main { index: updatedIndex
                  , showUserArea: false
                  , cardManagerState: cardManagerState {cardViewState = Card newCard newCardEntry, selectedEntry = Just newCardEntry}
                  , userPasswordGeneratorSettings 
                  }
            )
          )
        )

editCardSteps :: CardManagerState -> StatelessAppState -> CardEntry -> DataModel.Card.Card -> DataModel.Card.Card -> Page -> ExceptT AppError (Widget HTML) OperationState
editCardSteps cardManagerState state@{index, hash: hashFunc, userPreferences, cardsCache} oldCardEntry oldCard updatedCard page = do
  index'                                 <- except $ note (InvalidStateError $ CorruptedState "index not found")             index
  userPasswordGeneratorSettings          <- except $ note (InvalidStateError $ CorruptedState "user preferences not found") (userPreferences <#> (\up -> (unwrap up).passwordGeneratorSettings))

  ProxyResponse proxy'   cardEntry       <- runStep (postCard state updatedCard)                                                         (WidgetState (spinnerOverlay "Post updated card") page)
  let updatedIndex                        = addToIndex cardEntry >>> removeFromIndex oldCardEntry $ index'
  ProxyResponse proxy''  stateUpdateInfo <- runStep (updateIndex (state {proxy = proxy'}) updatedIndex)                                  (WidgetState (spinnerOverlay "Update index")      page)
  ProxyResponse proxy''' _               <- runStep (deleteCard  {proxy: proxy'', hashFunc} (unwrap oldCardEntry).cardReference oldCard) (WidgetState (spinnerOverlay "Delete old card")   page)
  let updatedCardCache                    = insert (reference cardEntry) updatedCard >>> delete (reference oldCardEntry) $ cardsCache

  pure (Tuple 
    (state {proxy = proxy''', index = Just updatedIndex, userInfoReferences = Just stateUpdateInfo.newUserInfoReferences, masterKey = Just stateUpdateInfo.newMasterKey, cardsCache = updatedCardCache})
    (WidgetState
      hiddenOverlayInfo
      (Main { index: updatedIndex, showUserArea: false, cardManagerState: cardManagerState {cardViewState = (Card updatedCard cardEntry), selectedEntry = Just cardEntry}, userPasswordGeneratorSettings })
    )
  )

-- ===================================================================================================

runStep :: forall a. ExceptT AppError Aff a -> WidgetState -> ExceptT AppError (Widget HTML) a
runStep step widgetState = ExceptT $ (step # runExceptT # liftAff) <|> (defaultView widgetState)
-- runStep step widgetState = ExceptT $ ((step # runExceptT # liftAff) <* (liftAff $ delay (Milliseconds 1000.0))) <|> (defaultView widgetState)

defaultView :: forall a. WidgetState -> Widget HTML a
defaultView widgetState = (unsafeCoerce unit <$ appView widgetState)

type MaxPinAttemptsReached = Boolean

handlePinResult :: StatelessAppState -> Page -> Either AppError OperationState -> Widget HTML (Tuple Page (Either AppError OperationState))
handlePinResult state page either = do
  storage <- liftEffect $ window >>= localStorage
  newPage <- case either of
    Right _ -> ( do
        liftEffect $ setItem (makeKey "failures") (show 0) storage
        pure $ page 
      ) <|> (defaultView (WidgetState {status: Spinner, message: "Reset PIN attempts"} page))
    Left  _ -> ( do
        failures <- liftEffect $ getItem (makeKey "failures") storage
        let count = (((fromMaybe 0) <<< fromString <<< (fromMaybe "")) failures) + 1
        if count < 3 then do
          liftEffect $ setItem (makeKey "failures") (show count) storage
          pure $ Login $ emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = PinLogin}
        else do
          liftEffect $ deleteCredentials storage
          pure $ Login $ emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = CredentialLogin}
      ) <|> (defaultView (WidgetState {status: Spinner, message: "Compute PIN attempts"} page))

  pure $ Tuple newPage either

defaultErrorPage :: Page
defaultErrorPage = Login emptyLoginFormData

handleOperationResult :: StatelessAppState -> Page -> Either AppError OperationState -> Widget HTML OperationState
handleOperationResult state page = either
                                        manageError
                                        (\res@(Tuple _ (WidgetState _ page')) -> delayOperation 500 (WidgetState { status: Done, message: "" } page') *> pure res)
                                                
  where
    manageError :: AppError -> Widget HTML OperationState
    manageError error = 
      case error of
        -- _ -> ErrorPage --TODO
        err -> do
          liftEffect $ log $ show err
          delayOperation 500 (WidgetState { status: Failed,  message: "error" } page)
          pure $ Tuple state (WidgetState { status: Hidden,  message: ""      } page)

delayOperation :: Int -> WidgetState -> Widget HTML Unit
delayOperation time widgetState = ((liftAff $ delay (Milliseconds $ toNumber time)) <|> (unit <$ appView widgetState))

-- ========================================================================================================================

-- runStepWithStateT :: forall a. ExceptT AppError (StateT StatelessAppState Aff) a -> WidgetState -> ExceptT AppError (StateT StatelessAppState (Widget HTML)) a
-- runStepWithStateT step widgetState = step # mapExceptT (mapStateT (\res -> liftAff res <|> (defaultView widgetState)))

-- handleOperationResultWithStateT :: Page -> Tuple (Either AppError WidgetState) StatelessAppState -> Widget HTML OperationState
-- handleOperationResultWithStateT page res = swap res <#> either
--                                             manageError
--                                             (\widgetState@(WidgetState _ page') -> delayOperation 500 (WidgetState { status: Done, message: "" } page') *> pure widgetState)
--                                          # sequence
                                                
--   where
--     manageError :: AppError -> Widget HTML WidgetState
--     manageError error = 
--       case error of
--         -- _ -> ErrorPage --TODO
--         err -> do
--           liftEffect $ log $ show err
--           delayOperation 500 (WidgetState { status: Failed,  message: "error" } page)
--           pure               (WidgetState { status: Hidden,  message: ""      } page)

-- addCardStepsWithStateT :: DataModel.Card.Card -> Page -> String -> ExceptT AppError (StateT StatelessAppState (Widget HTML)) WidgetState
-- addCardStepsWithStateT newCard page message = do
--   {index, userPreferences, cardsCache} <- get
--   index'                               <-  except $ note (InvalidStateError $ CorruptedState "index not found")             index
--   userPasswordGeneratorSettings        <-  except $ note (InvalidStateError $ CorruptedState "user preferences not found") (userPreferences <#> (\up -> (unwrap up).passwordGeneratorSettings))

--   newCardEntry                          <- runStepWithStateT (postCardWithStateT newCard)         (WidgetState (spinnerOverlay message)        page)
--   let updatedIndex                       = addToIndex newCardEntry index'
--   _                                     <- runStepWithStateT (updateIndexWithStateT updatedIndex) (WidgetState (spinnerOverlay "Update index") page)
--   let updatedCardCache                   = insert (reference newCardEntry) newCard cardsCache
  
--   modify_ (\state -> state {cardsCache = updatedCardCache})

--   pure  (WidgetState
--           hiddenOverlayInfo
--           (Main { index: updatedIndex
--                 , showUserArea: false
--                 , cardsManagerState: cardManagerInitialState {cardViewState = Card newCard newCardEntry, selectedEntry = Just newCardEntry}
--                 , userPasswordGeneratorSettings 
--                 }
--           )
--         )

-- postCardWithStateT :: DataModel.Card.Card -> ExceptT AppError (StateT StatelessAppState Aff) CardEntry
-- postCardWithStateT card = do
--   {proxy, hash} <- get
--   key <- liftAff $ KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
--   Tuple encryptedCard cardEntry@(CardEntry {cardReference: CardReference {reference}}) <- liftAff $ createCardEntry card key hashFuncSHA256
--   ProxyResponse proxy' _ <- mapExceptT lift $ postStatelessBlob {proxy, hashFunc: hash} encryptedCard (toArrayBuffer reference)
--   modify_ (\state -> state {proxy = proxy'})
--   pure cardEntry

-- updateIndexWithStateT :: Index -> ExceptT AppError (StateT StatelessAppState Aff) Unit
-- updateIndexWithStateT newIndex = do
--   state <- get
--   case state of
--     { c: Just c, p: Just p, userInfoReferences: Just (UserInfoReferences r@{ indexReference: (IndexReference oldReference) }), masterKey: Just originMasterKey, proxy, hash: hashFunc, index: Just index } -> do
--       cryptoKey            :: CryptoKey   <- liftAff $ cryptoKeyAES (toArrayBuffer oldReference.masterKey)
--       indexCardContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newIndex
--       indexCardContentHash :: ArrayBuffer <- liftAff $ hashFunc (indexCardContent : Nil)
--       ProxyResponse proxy'   _            <- mapExceptT lift $ postStatelessBlob {proxy, hashFunc} indexCardContent indexCardContentHash
--       modify_ (\state_ -> state_ {proxy = proxy'})
--       -- -------------------
--       let newIndexReference                = IndexReference $ oldReference { reference = fromArrayBuffer indexCardContentHash }
--       let newUserInfoReferences            = UserInfoReferences r { indexReference = newIndexReference }
--       masterPassword       :: CryptoKey   <- liftAff $ cryptoKeyAES (toArrayBuffer p)
--       masterKeyContent     :: HexString   <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword newUserInfoReferences
--       let newUserCard                      = UserCard { masterKey: Tuple masterKeyContent V_1, originMasterKey: fst originMasterKey }
--       ProxyResponse proxy'' newMasterKey  <- mapExceptT lift $ updateUserCard {proxy: proxy', hashFunc} c newUserCard
--       modify_ (\state_ -> state_ {proxy = proxy''})
--       -- -------------------
--       oldIndexCartContent  :: ArrayBuffer <- liftAff $ encryptJson cryptoKey index

--       ProxyResponse proxy''' _            <- mapExceptT lift $ deleteStatelessBlob {proxy: proxy'', hashFunc} oldIndexCartContent oldReference.reference
--       modify_ (\state_ -> state_ { proxy = proxy'''
--                                , index = Just newIndex
--                                , userInfoReferences = Just newUserInfoReferences
--                                , masterKey = Just newMasterKey
--                                })

--     _ -> throwError $ InvalidStateError (MissingValue "Missing p, c or indexReference")
