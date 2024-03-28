module Functions.Handler.CardManagerEventHandler
  ( getCardSteps
  , handleCardManagerEvent
  , loadingMainPage
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT, runExceptT, throwError)
import Data.Function ((#), ($))
import Data.Map (insert, lookup)
import Data.Maybe (Maybe(..), isJust, isNothing)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.AppState (AppState, CardsCache, InvalidStateError(..), ProxyResponse(..))
import DataModel.CardVersions.Card as DataModel.CardVersions.Card
import DataModel.FragmentState as Fragment
import DataModel.IndexVersions.Index (CardEntry(..), Index, addToIndex, reference, removeFromIndex)
import DataModel.UserVersions.User (UserInfo(..))
import DataModel.WidgetState (CardFormInput(..), CardManagerState, CardViewState(..), Page(..), WidgetState(..), MainPageWidgetState)
import Effect.Aff.Class (liftAff)
import Functions.Card (appendToTitle, archiveCard, decryptCard, restoreCard)
import Functions.Communication.Backend (ConnectionState)
import Functions.Communication.Blobs (getBlob)
import Functions.Communication.Cards (deleteCard, postCard)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, noOperation, handleOperationResult, runStep)
import Functions.Index (updateIndex)
import Record (merge)
import Views.AppView (emptyMainPageWidgetState)
import Views.CardsManagerView (CardManagerEvent(..), NavigateCardsEvent(..))
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.UserAreaView (userAreaInitialState)

loadingMainPage :: Index -> CardManagerState -> Page
loadingMainPage index cardManagerState = Main emptyMainPageWidgetState { index = index, cardManagerState = cardManagerState }

handleCardManagerEvent :: CardManagerEvent -> CardManagerState -> AppState -> Fragment.FragmentState -> Widget HTML OperationState
handleCardManagerEvent cardManagerEvent cardManagerState state@{index: Just index, userInfo: Just (UserInfo {userPreferences}), proxy, srpConf, hash: hashFunc, c: Just c, p: Just p, username: Just username, password: Just password, pinEncryptedPassword, cardsCache, donationLevel: Just donationLevel} _ = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  
  let defaultPage = { index
                    , credentials:      {username, password}
                    , pinExists:        isJust pinEncryptedPassword
                    , userPreferences
                    , userAreaState: userAreaInitialState
                    , cardManagerState
                    , donationLevel
                    }

  case cardManagerEvent of

    (OpenUserAreaEvent) -> 
      noOperation (Tuple 
                  state
                  (WidgetState
                    hiddenOverlayInfo
                    (Main defaultPage { userAreaState = userAreaInitialState {showUserArea = true} }
                    )
                  )
                )

    (ShowShortcutsEvent show) ->
      updateCardManagerState defaultPage cardManagerState {showShortcutsHelp = show}

    (ChangeFilterEvent filterData) ->
      updateCardManagerState defaultPage cardManagerState { filterData = filterData
                                                          , highlightedEntry = Nothing 
                                                          }

    (NavigateCardsEvent navigationEvent) ->
      case navigationEvent of
        Move             i -> updateCardManagerState defaultPage (cardManagerState {                                 highlightedEntry = Just i })
        Close       maybei -> updateCardManagerState defaultPage (cardManagerState {cardViewState = NoCard,          highlightedEntry = maybei })
        Open   Nothing     -> doNothing defaultPage
        Open  (Just entry) -> do
          ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps connectionState cardsCache entry (Main defaultPage)
          pure (Tuple
                  state {proxy = proxy', cardsCache = cardsCache'}
                  (WidgetState
                    hiddenOverlayInfo
                    (Main defaultPage { cardManagerState =  cardManagerState {cardViewState = Card card entry, highlightedEntry = Nothing} }
                    )
                  )
                )
          # runExceptT
          >>= handleOperationResult state defaultErrorPage (isNothing $ lookup (reference entry) cardsCache) Black
    
    (OpenCardFormEvent maybeCard) ->
      updateCardManagerState defaultPage cardManagerState { cardViewState = CardForm $ case maybeCard of
                                                              Nothing                     -> NewCard
                                                              Just (Tuple cardEntry card) -> ModifyCard card cardEntry
                                                          }

    (AddCardEvent card) ->
      do
        res <- addCardSteps cardManagerState state card (loadingMainPage index cardManagerState {cardViewState = CardForm (NewCardFromFragment card)}) "Add card"
        pure res
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    (CloneCardEvent cardEntry) ->
      do
        ProxyResponse proxy' (Tuple cardsCache' card) <- getCardSteps connectionState cardsCache cardEntry (loadingMainPage index cardManagerState)
        let cloneCard                                  = appendToTitle " - copy" <<< (\(DataModel.CardVersions.Card.Card card') -> DataModel.CardVersions.Card.Card card' {archived = false}) $ card
        res                                           <- addCardSteps cardManagerState state{proxy = proxy', cardsCache = cardsCache'} cloneCard (loadingMainPage index cardManagerState) "Clone card"
        pure res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black
  
    (DeleteCardEvent cardEntry) ->
      do
        ProxyResponse proxy'  cardsCache'     <- runStep (deleteCard connectionState cardsCache (unwrap cardEntry).cardReference) (WidgetState (spinnerOverlay "Delete card"  Black) (loadingMainPage index cardManagerState))
        updatedIndex                          <- runStep ((removeFromIndex cardEntry index) # liftAff)                            (WidgetState (spinnerOverlay "Update index" Black) (loadingMainPage index cardManagerState))
        ProxyResponse proxy'' stateUpdateInfo <- runStep (updateIndex (state {proxy = proxy'}) updatedIndex)                      (WidgetState (spinnerOverlay "Update index" Black) (loadingMainPage index cardManagerState))

        pure (Tuple 
                (merge stateUpdateInfo state {proxy = proxy'', index = Just updatedIndex, cardsCache = cardsCache'})
                (WidgetState
                  hiddenOverlayInfo
                  (Main defaultPage { index            = updatedIndex
                                    , cardManagerState = cardManagerState {cardViewState = NoCard, highlightedEntry = Nothing}
                                    }
                  )
                )
            )

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    (EditCardEvent (Tuple oldCardEntry updatedCard)) ->
      do
        editCardSteps cardManagerState state oldCardEntry updatedCard (loadingMainPage index cardManagerState {cardViewState = CardForm (ModifyCard updatedCard oldCardEntry) })
      
      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

    (ArchiveCardEvent cardEntry) ->
      do
        ProxyResponse proxy' (Tuple _ card) <- getCardSteps  connectionState  cardsCache            cardEntry             (loadingMainPage index cardManagerState)
        let updatedCard                      = archiveCard card
        res                                 <- editCardSteps cardManagerState state{proxy = proxy'} cardEntry updatedCard (loadingMainPage index cardManagerState)
        pure res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black
    
    (RestoreCardEvent cardEntry) ->
      do
        ProxyResponse proxy' (Tuple _ card) <- getCardSteps  connectionState  cardsCache            cardEntry             (loadingMainPage index cardManagerState)
        let updatedCard                      = restoreCard card
        res                                 <- editCardSteps cardManagerState state{proxy = proxy'} cardEntry updatedCard (loadingMainPage index cardManagerState)
        pure res

      # runExceptT
      >>= handleOperationResult state defaultErrorPage true Black

  where
    updateCardManagerState :: MainPageWidgetState -> CardManagerState -> Widget HTML OperationState
    updateCardManagerState mainPageState cardManagerState' = 
      noOperation (Tuple 
                    state
                    (WidgetState
                      hiddenOverlayInfo
                      (Main mainPageState { cardManagerState = cardManagerState' }
                      )
                    )
                  )
    
    doNothing :: MainPageWidgetState -> Widget HTML OperationState
    doNothing mainPageState =  noOperation (Tuple state (WidgetState hiddenOverlayInfo (Main mainPageState)))


handleCardManagerEvent _ _ state _ = do
  throwError $ InvalidStateError (CorruptedState "cardManagerEvent")
  # runExceptT
  >>= handleOperationResult state defaultErrorPage true Black

-- ===================================================================================================

getCardSteps :: ConnectionState -> CardsCache -> CardEntry -> Page -> ExceptT AppError (Widget HTML) (ProxyResponse (Tuple CardsCache DataModel.CardVersions.Card.Card))
getCardSteps connectionState cardsCache cardEntry@(CardEntry entry) page = do
  let cardFromCache = lookup (reference cardEntry) cardsCache
  case cardFromCache of
    Just card -> pure $ ProxyResponse connectionState.proxy (Tuple cardsCache card)
    Nothing   -> do
      ProxyResponse proxy' blob <- runStep (getBlob connectionState (reference cardEntry)) (WidgetState (spinnerOverlay "Get card"     Black) page)  
      card                      <- runStep (decryptCard blob (entry.cardReference))        (WidgetState (spinnerOverlay "Decrypt card" Black) page)
      let updatedCardsCache      =          insert (reference cardEntry) card cardsCache
      pure $ ProxyResponse proxy' (Tuple updatedCardsCache card)

addCardSteps :: CardManagerState -> AppState -> DataModel.CardVersions.Card.Card -> Page -> String -> ExceptT AppError (Widget HTML) OperationState
addCardSteps cardManagerState state@{index: Just index, userInfo: Just (UserInfo {userPreferences}), proxy, hash: hashFunc, srpConf, c: Just c, p: Just p, cardsCache, username: Just username, password: Just password, pinEncryptedPassword, donationLevel: Just donationLevel} newCard page message = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  ProxyResponse proxy'  (Tuple cardsCache' newCardEntry) <- runStep (postCard connectionState cardsCache newCard)       (WidgetState (spinnerOverlay message        Black) page)
  updatedIndex                                           <- runStep (addToIndex newCardEntry index # liftAff)           (WidgetState (spinnerOverlay "Update index" Black) page)
  ProxyResponse proxy''  stateUpdateInfo                 <- runStep (updateIndex (state {proxy = proxy'}) updatedIndex) (WidgetState (spinnerOverlay "Update index" Black) page)

  pure (Tuple 
          (merge stateUpdateInfo state  {proxy = proxy'', index = Just updatedIndex, cardsCache = cardsCache'})
          (WidgetState
            hiddenOverlayInfo
            (Main { index:            updatedIndex
                  , credentials:     {username, password}
                  , pinExists: isJust pinEncryptedPassword
                  , userPreferences
                  , userAreaState:    userAreaInitialState
                  , cardManagerState: cardManagerState {cardViewState = Card newCard newCardEntry, highlightedEntry = Nothing}
                  , donationLevel
                  }
            )
          )
        )
addCardSteps _ _ _ _ _ = throwError $ InvalidStateError (CorruptedState "addCardStep")

editCardSteps :: CardManagerState -> AppState -> CardEntry -> DataModel.CardVersions.Card.Card -> Page -> ExceptT AppError (Widget HTML) OperationState
editCardSteps cardManagerState state@{index: Just index, proxy, srpConf, hash: hashFunc, c: Just c, p: Just p, userInfo: Just (UserInfo {userPreferences}), cardsCache, username: Just username, password: Just password, pinEncryptedPassword, donationLevel: Just donationLevel} oldCardEntry updatedCard page = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  ProxyResponse proxy'   (Tuple cardsCache' cardEntry) <- runStep  (postCard connectionState cardsCache updatedCard)                                             (WidgetState (spinnerOverlay "Post updated card" Black) page)
  ProxyResponse proxy''         cardsCache''           <- runStep  (deleteCard  connectionState{proxy = proxy'} cardsCache' (unwrap oldCardEntry).cardReference) (WidgetState (spinnerOverlay "Delete old card"   Black) page)
  updatedIndex                                         <- runStep ((addToIndex cardEntry index >>= removeFromIndex oldCardEntry) # liftAff)                      (WidgetState (spinnerOverlay "Update index"      Black) page)
  ProxyResponse proxy'''  stateUpdateInfo              <- runStep  (updateIndex (state {proxy = proxy''}) updatedIndex)                                          (WidgetState (spinnerOverlay "Update index"      Black) page)

  pure  (Tuple 
          (merge stateUpdateInfo state {proxy = proxy''', index = Just updatedIndex, cardsCache = cardsCache''})
          (WidgetState
            hiddenOverlayInfo
            (Main { index:            updatedIndex
                  , credentials:     {username, password}
                  , pinExists: isJust pinEncryptedPassword
                  , userPreferences
                  , userAreaState:    userAreaInitialState
                  , cardManagerState: cardManagerState {cardViewState = (Card updatedCard cardEntry), highlightedEntry = Nothing}
                  , donationLevel
                  }
            )
          )
        )
editCardSteps _ _ _ _ _ = throwError $ InvalidStateError (CorruptedState "editCardStep")


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

-- addCardStepsWithStateT :: DataModel.CardVersions.Card.Card -> Page -> String -> ExceptT AppError (StateT AppState (Widget HTML)) WidgetState
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

-- postCardWithStateT :: DataModel.CardVersions.Card.Card -> ExceptT AppError (StateT AppState Aff) CardEntry
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
--       cryptoKey            :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer oldReference.masterKey)
--       indexCardContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newIndex
--       indexCardContentHash :: ArrayBuffer <- liftAff $ hashFunc (indexCardContent : Nil)
--       ProxyResponse proxy'   _            <- mapExceptT lift $ postBlob {proxy, hashFunc} indexCardContent indexCardContentHash
--       modify_ (\state_ -> state_ {proxy = proxy'})
--       -- -------------------
--       let newIndexReference                = IndexReference $ oldReference { reference = fromArrayBuffer indexCardContentHash }
--       let newUserInfoReferences            = UserInfoReferences r { indexReference = newIndexReference }
--       masterPassword       :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer p)
--       masterKeyContent     :: HexString   <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword newUserInfoReferences
--       let newUserCard                      = UserCard { masterKey: Tuple masterKeyContentMasterKeyEncodingVersion_1, originMasterKey: fst originMasterKey }
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
