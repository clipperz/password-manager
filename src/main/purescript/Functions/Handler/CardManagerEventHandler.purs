module Functions.Handler.CardManagerEventHandler where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Except (except)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Either (note)
import Data.Function ((#), ($))
import Data.Map (delete, insert, lookup)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Card as DataModel.Card
import DataModel.FragmentState as Fragment
import DataModel.Index (CardEntry(..), addToIndex, emptyIndex, reference, removeFromIndex)
import DataModel.Password (standardPasswordGeneratorSettings)
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import Functions.Card (appendToTitle, archiveCard, getCardContent, restoreCard)
import Functions.Communication.Blobs (getStatelessBlob)
import Functions.Communication.Cards (deleteCard, postCard)
import Functions.Communication.Users (updateIndex)
import Functions.Handler.GenericHandlerFunctions (OperationState, defaultErrorPage, doNothing, handleOperationResult, runStep)
import Views.AppView (Page(..), WidgetState(..), loadingMainPage)
import Views.CardsManagerView (CardManagerEvent(..), CardManagerState, CardViewState(..))
import Views.OverlayView (hiddenOverlayInfo, spinnerOverlay)

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

-- ===================================================================================================

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
