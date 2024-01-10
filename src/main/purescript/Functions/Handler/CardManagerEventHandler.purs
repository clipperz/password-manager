module Functions.Handler.CardManagerEventHandler
  ( getCardSteps
  , handleCardManagerEvent
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Function ((#), ($))
import Data.Map (delete, insert, lookup)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.AppState (AppState, CardsCache, InvalidStateError(..), ProxyResponse(..))
import DataModel.Card as DataModel.Card
import DataModel.FragmentState as Fragment
import DataModel.Index (CardEntry(..), Index, addToIndex, reference, removeFromIndex)
import Functions.Card (appendToTitle, archiveCard, getCardContent, restoreCard)
import Functions.Communication.Backend (ConnectionState)
import Functions.Communication.Blobs (getBlob)
import Functions.Communication.Cards (deleteCard, postCard)
import Functions.Communication.Users (updateIndex)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, doNothing, handleOperationResult, runStep)
import Views.AppView (Page(..), WidgetState(..), emptyMainPageWidgetState)
import Views.CardsManagerView (CardManagerEvent(..), CardManagerState, CardViewState(..))
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (userAreaInitialState)

loadingMainPage :: Index -> CardManagerState -> Page
loadingMainPage index cardManagerState = Main emptyMainPageWidgetState { index = index, cardManagerState = cardManagerState }

handleCardManagerEvent :: CardManagerEvent -> CardManagerState -> AppState -> Fragment.FragmentState -> Widget HTML OperationState

handleCardManagerEvent OpenUserAreaEvent cardManagerState state@{index: Just index, userPreferences: Just userPreferences, username: Just username, password: Just password, pinEncryptedPassword} _ = 
  doNothing (Tuple 
              state
              (WidgetState
                hiddenOverlayInfo
                (Main { index
                      , credentials:   {username, password}
                      , pinExists: isJust pinEncryptedPassword
                      , userAreaState: userAreaInitialState {showUserArea = true}
                      , cardManagerState
                      , userPreferences
                      }
                )
              )
            )


handleCardManagerEvent (OpenCardViewEvent cardEntry) cardManagerState state@{index: Just index, userPreferences: Just userPreferences, proxy, srpConf, hash: hashFunc, c: Just c, p: Just p, username: Just username, password: Just password, pinEncryptedPassword, cardsCache} _ = 
  do
    let connectionState = {proxy, hashFunc, srpConf, c, p}
    ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps connectionState cardsCache cardEntry (Main { index, credentials: {username, password}, pinExists: isJust pinEncryptedPassword, userAreaState: userAreaInitialState, cardManagerState, userPreferences})
    pure (Tuple
            state {proxy = proxy', cardsCache = cardsCache'}
            (WidgetState
              hiddenOverlayInfo
              (Main { index
                    , credentials:      {username, password}
                    , pinExists: isJust pinEncryptedPassword
                    , userPreferences
                    , userAreaState:    userAreaInitialState
                    , cardManagerState: cardManagerState {selectedEntry = (Just cardEntry), cardViewState = Card card cardEntry}
                    }
              )
            )
          )

  # runExceptT
  >>= handleOperationResult state defaultErrorPage (isNothing $ lookup (reference cardEntry) cardsCache) Black


handleCardManagerEvent (AddCardEvent card) cardManagerState state@{index: Just index} _ = 
  do
    res <- addCardSteps cardManagerState state card (loadingMainPage index cardManagerState) "Add card"
    pure res

  # runExceptT
  >>= handleOperationResult state defaultErrorPage true Black
    
  --   res    <- addCardStepsWithStateT card (loadingMainPage index' (CardForm $ NewCard $ Just card)) "Add card"
  --   pure res

  -- # runExceptT
  -- # flip runStateT state
  -- >>= handleOperationResultWithStateT defaultErrorPage

handleCardManagerEvent (CloneCardEvent cardEntry) cardManagerState state@{index: Just index, srpConf, proxy, hash: hashFunc, cardsCache, c: Just c, p: Just p} _ = 
  do
    let connectionState = {proxy, hashFunc, srpConf, c, p}
    ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps connectionState cardsCache cardEntry (loadingMainPage index cardManagerState)
    let cloneCard      = appendToTitle " - copy" <<< (\(DataModel.Card.Card card') -> DataModel.Card.Card card' {archived = false}) $ card
    res               <- addCardSteps cardManagerState state{proxy = proxy', cardsCache = cardsCache'} cloneCard (loadingMainPage index cardManagerState) "Clone card"
    pure res

  # runExceptT
  >>= handleOperationResult state defaultErrorPage true Black


handleCardManagerEvent (DeleteCardEvent cardEntry) cardManagerState state@{hash: hashFunc, srpConf, proxy, c: Just c, p: Just p, cardsCache, index: Just index, userPreferences: Just userPreferences, username: Just username, password: Just password, pinEncryptedPassword} _ =
  do
    let connectionState = {proxy, hashFunc, srpConf, c, p}
    ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps connectionState cardsCache cardEntry (loadingMainPage index cardManagerState)
    ProxyResponse proxy'' _                <- runStep (deleteCard connectionState{proxy = proxy'} (unwrap cardEntry).cardReference card) (WidgetState (spinnerOverlay "Delete card"  Black) (loadingMainPage index cardManagerState))
    let updatedIndex                       = removeFromIndex cardEntry index
    ProxyResponse proxy''' stateUpdateInfo <- runStep (updateIndex (state {proxy = proxy''}) updatedIndex)                 (WidgetState (spinnerOverlay "Update index" Black) (loadingMainPage index cardManagerState))

    pure (Tuple 
            (state {proxy = proxy''', cardsCache = cardsCache', index = Just updatedIndex, userInfoReferences = Just stateUpdateInfo.newUserInfoReferences, masterKey = Just stateUpdateInfo.newMasterKey})
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
  >>= handleOperationResult state defaultErrorPage true Black


handleCardManagerEvent (EditCardEvent oldCardEntry updatedCard) cardManagerState state@{index: Just index, srpConf, proxy, hash: hashFunc, cardsCache, c: Just c, p: Just p} _ =
  do
    let connectionState = {proxy, hashFunc, srpConf, c, p}
    ProxyResponse proxy' (Tuple cardsCache' oldCard) <- getCardSteps                   connectionState cardsCache                     oldCardEntry                     (loadingMainPage index cardManagerState)
    res                                              <- editCardSteps cardManagerState state{proxy = proxy', cardsCache = cardsCache'} oldCardEntry oldCard updatedCard (loadingMainPage index cardManagerState)
    pure res
  
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true Black


handleCardManagerEvent (ArchiveCardEvent cardEntry) cardManagerState state@{index: Just index, srpConf, proxy, hash: hashFunc, cardsCache, c: Just c, p: Just p} _ =
  do
    let connectionState = {proxy, hashFunc, srpConf, c, p}
    ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps                   connectionState cardsCache                     cardEntry                  (loadingMainPage index cardManagerState)
    let updatedCard    = archiveCard card
    res                                           <- editCardSteps cardManagerState state{proxy = proxy', cardsCache = cardsCache'} cardEntry card updatedCard (loadingMainPage index cardManagerState)
    pure res

  # runExceptT
  >>= handleOperationResult state defaultErrorPage true Black


handleCardManagerEvent (RestoreCardEvent cardEntry) cardManagerState state@{index: Just index, srpConf, proxy, hash: hashFunc, cardsCache, c: Just c, p: Just p} _ =
  do
    let connectionState = {proxy, hashFunc, srpConf, c, p}
    ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps                   connectionState cardsCache                     cardEntry                  (loadingMainPage index cardManagerState)
    let updatedCard    = restoreCard card
    res                                           <- editCardSteps cardManagerState state{proxy = proxy', cardsCache = cardsCache'} cardEntry card updatedCard (loadingMainPage index cardManagerState)
    pure res

  # runExceptT
  >>= handleOperationResult state defaultErrorPage true Black

handleCardManagerEvent _ _ state _ = do
  throwError $ InvalidStateError (CorruptedState "State is corrupted")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true Black

-- ===================================================================================================

getCardSteps :: ConnectionState -> CardsCache -> CardEntry -> Page -> ExceptT AppError (Widget HTML) (ProxyResponse (Tuple CardsCache DataModel.Card.Card))
getCardSteps connectionState cardsCache cardEntry@(CardEntry entry) page = do
  let cardFromCache = lookup (reference cardEntry) cardsCache
  case cardFromCache of
    Just card -> pure $ ProxyResponse connectionState.proxy (Tuple cardsCache card)
    Nothing   -> do
      ProxyResponse proxy' blob <- runStep (getBlob connectionState (reference cardEntry)) (WidgetState (spinnerOverlay "Get card"     Black) page)  
      card                      <- runStep (getCardContent blob (entry.cardReference))     (WidgetState (spinnerOverlay "Decrypt card" Black) page)
      let updatedCardsCache = insert (reference cardEntry) card cardsCache
      pure $ ProxyResponse proxy' (Tuple updatedCardsCache card)

addCardSteps :: CardManagerState -> AppState -> DataModel.Card.Card -> Page -> String -> ExceptT AppError (Widget HTML) OperationState
addCardSteps cardManagerState state@{index: Just index, userPreferences: Just userPreferences, proxy, hash: hashFunc, srpConf, c: Just c, p: Just p, cardsCache, username: Just username, password: Just password, pinEncryptedPassword} newCard page message = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  ProxyResponse proxy'  newCardEntry    <- runStep (postCard connectionState newCard)                  (WidgetState (spinnerOverlay message        Black) page)
  let updatedIndex                       = addToIndex newCardEntry index
  ProxyResponse proxy'' stateUpdateInfo <- runStep (updateIndex (state {proxy = proxy'}) updatedIndex) (WidgetState (spinnerOverlay "Update index" Black) page)
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
            (Main { index:            updatedIndex
                  , credentials:      {username, password}
                  , pinExists: isJust pinEncryptedPassword
                  , userPreferences
                  , userAreaState:    userAreaInitialState
                  , cardManagerState: cardManagerState {cardViewState = Card newCard newCardEntry, selectedEntry = Just newCardEntry}
                  }
            )
          )
        )
addCardSteps _ _ _ _ _ = throwError $ InvalidStateError (CorruptedState "State is corrupted")

editCardSteps :: CardManagerState -> AppState -> CardEntry -> DataModel.Card.Card -> DataModel.Card.Card -> Page -> ExceptT AppError (Widget HTML) OperationState
editCardSteps cardManagerState state@{index: Just index, proxy, srpConf, hash: hashFunc, c: Just c, p: Just p, userPreferences: Just userPreferences, cardsCache, username: Just username, password: Just password, pinEncryptedPassword} oldCardEntry oldCard updatedCard page = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  ProxyResponse proxy'   cardEntry       <- runStep (postCard connectionState updatedCard)                                                     (WidgetState (spinnerOverlay "Post updated card" Black) page)
  let updatedIndex                        = addToIndex cardEntry >>> removeFromIndex oldCardEntry $ index
  ProxyResponse proxy''  stateUpdateInfo <- runStep (updateIndex (state {proxy = proxy'})         updatedIndex)                                (WidgetState (spinnerOverlay "Update index"      Black) page)
  ProxyResponse proxy''' _               <- runStep (deleteCard  connectionState{proxy = proxy''} (unwrap oldCardEntry).cardReference oldCard) (WidgetState (spinnerOverlay "Delete old card"   Black) page)
  let updatedCardCache                    = insert (reference cardEntry) updatedCard >>> delete (reference oldCardEntry) $ cardsCache

  pure (Tuple 
    (state {proxy = proxy''', index = Just updatedIndex, userInfoReferences = Just stateUpdateInfo.newUserInfoReferences, masterKey = Just stateUpdateInfo.newMasterKey, cardsCache = updatedCardCache})
    (WidgetState
      hiddenOverlayInfo
      (Main { index:            updatedIndex
            , credentials:      {username, password}
            , pinExists: isJust pinEncryptedPassword
            , userPreferences
            , userAreaState:    userAreaInitialState
            , cardManagerState: cardManagerState {cardViewState = (Card updatedCard cardEntry), selectedEntry = Just cardEntry}
            }
      )
    )
  )
editCardSteps _ _ _ _ _ _ = throwError $ InvalidStateError (CorruptedState "State is corrupted")

-- ========================================================================================================================

-- runStepWithStateT :: forall a. ExceptT AppError (StateT AppState Aff) a -> WidgetState -> ExceptT AppError (StateT AppState (Widget HTML)) a
-- runStepWithStateT step widgetState = step # mapExceptT (mapStateT (\res -> liftAff res <|> (defaultView widgetState)))

-- handleOperationResultWithStateT :: Page -> Tuple (Either AppError WidgetState) AppState -> Widget HTML OperationState
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

-- addCardStepsWithStateT :: DataModel.Card.Card -> Page -> String -> ExceptT AppError (StateT AppState (Widget HTML)) WidgetState
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

-- postCardWithStateT :: DataModel.Card.Card -> ExceptT AppError (StateT AppState Aff) CardEntry
-- postCardWithStateT card = do
--   {proxy, hash} <- get
--   key <- liftAff $ KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
--   Tuple encryptedCard cardEntry@(CardEntry {cardReference: CardReference {reference}}) <- liftAff $ createCardEntry card key hashFuncSHA256
--   ProxyResponse proxy' _ <- mapExceptT lift $ postBlob {proxy, hashFunc: hash} encryptedCard (toArrayBuffer reference)
--   modify_ (\state -> state {proxy = proxy'})
--   pure cardEntry

-- updateIndexWithStateT :: Index -> ExceptT AppError (StateT AppState Aff) Unit
-- updateIndexWithStateT newIndex = do
--   state <- get
--   case state of
--     { c: Just c, p: Just p, userInfoReferences: Just (UserInfoReferences r@{ indexReference: (IndexReference oldReference) }), masterKey: Just originMasterKey, proxy, hash: hashFunc, index: Just index } -> do
--       cryptoKey            :: CryptoKey   <- liftAff $ cryptoKeyAES (toArrayBuffer oldReference.masterKey)
--       indexCardContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newIndex
--       indexCardContentHash :: ArrayBuffer <- liftAff $ hashFunc (indexCardContent : Nil)
--       ProxyResponse proxy'   _            <- mapExceptT lift $ postBlob {proxy, hashFunc} indexCardContent indexCardContentHash
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

--       ProxyResponse proxy''' _            <- mapExceptT lift $ deleteBlob {proxy: proxy'', hashFunc} oldIndexCartContent oldReference.reference
--       modify_ (\state_ -> state_ { proxy = proxy'''
--                                , index = Just newIndex
--                                , userInfoReferences = Just newUserInfoReferences
--                                , masterKey = Just newMasterKey
--                                })

--     _ -> throwError $ InvalidStateError (MissingValue "Missing p, c or indexReference")
