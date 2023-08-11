module OperationalWidgets.CardsManagerWidget
  ( cardsManagerWidget
  , CardsManagerAction(..)
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>), flap)
import Data.List ((:), filter)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..), InvalidStateError(..), ProxyConnectionStatus)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.FragmentData (FragmentData(..))
import DataModel.Index (Index(..), CardEntry(..))
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Users (updateIndex)
import Functions.JSState (getAppState, updateAppState)
import OperationalWidgets.CreateCardWidget (CardFormInput(..))
import Views.CardsManagerView (cardsManagerView, CardView(..), CardViewAction(..), CardViewState, CardsViewInfo, FilterViewStatus(..))
import Views.IndexView (IndexFilter(..), ComplexIndexFilter, addLastCardFilterInOr, removeAllLastCardFilter)

data CardsManagerAction = OpenUserArea

data CardsViewResult = CardsViewResult (Tuple CardsViewInfo CardViewAction) | OpResult Index CardViewState (Maybe AppError) ComplexIndexFilter

cardsManagerWidget :: ProxyConnectionStatus -> Index -> CardViewState -> Widget HTML CardsManagerAction
cardsManagerWidget proxyConnectionStatus ind _ = do
  getStateResult <- liftEffect $ getAppState
  _ <- updateAppState {fragmentData: Nothing}
  cardView <- pure $ case getStateResult of
    Left _  -> NoCard
    Right {fragmentData}  -> case fragmentData of
      Just (AddCard card) -> (CardForm (NewCard $ Just card))
      _                   -> NoCard
  let info = { index: ind
             , indexFilter: { archived: false, indexFilter: NoFilter }
             , selectedIndexPosition: Nothing
             , cardViewState: { cardView: cardView, cardViewState: Default }
             , error: Nothing
             }
  go info (cardsManagerView proxyConnectionStatus FilterViewClosed) Nothing

  where
    go :: CardsViewInfo -> (CardsViewInfo -> Widget HTML (Tuple CardsViewInfo CardViewAction)) -> Maybe (Aff CardsViewResult) -> Widget HTML CardsManagerAction
    go info view operation = do
      res <- case operation of
        Nothing -> CardsViewResult <$> (view info)
        Just op -> (CardsViewResult <$> (view info)) <|> (liftAff $ op)
      case res of
        CardsViewResult (Tuple f cva) -> case cva of 
          UpdateIndex updateData -> do
            go (getUpdateIndexInfo proxyConnectionStatus f updateData Nothing) view (Just (getUpdateIndexOp f updateData))
          ShowAddCard   -> go (info { cardViewState = {cardView: (CardForm $ NewCard Nothing), cardViewState: Default} }) view Nothing
          ShowUserArea  -> pure $ OpenUserArea
          ShowCard ref  -> go (info { cardViewState = {cardView: (CardFromReference ref), cardViewState: Default} }) view Nothing
        OpResult i cv e f -> go (info { index = i, indexFilter = f, error = e, cardViewState = cv }) view Nothing

getUpdateIndexOp :: CardsViewInfo -> IndexUpdateData -> Aff CardsViewResult
getUpdateIndexOp { index: index@(Index list), indexFilter } (IndexUpdateData action card) =
  case action of 
    AddReference                        entry -> flap (addEntryToIndex entry) { archived: false, indexFilter: ComposedOrFilter (SpecificCardFilter entry) indexFilter.indexFilter } 
    CloneReference                      entry -> flap (addEntryToIndex entry) (((addLastCardFilterInOr entry) <<< removeAllLastCardFilter) indexFilter)
    ChangeReferenceWithEdit    oldEntry entry -> flap (updateReferenceInIndex oldEntry entry) (((addLastCardFilterInOr entry) <<< removeAllLastCardFilter) indexFilter)
    ChangeReferenceWithoutEdit oldEntry entry -> flap (updateReferenceInIndex oldEntry entry) (((addLastCardFilterInOr entry) <<< removeAllLastCardFilter) indexFilter)
    DeleteReference            oldEntry       -> flap (removeReferenceFromIndex oldEntry) indexFilter
    NoUpdateNecessary          oldEntry       -> pure $ OpResult index { cardView: CardFromReference oldEntry, cardViewState: Default } Nothing indexFilter
    NoUpdate                                  -> pure $ OpResult index { cardView: fromMaybe NoCard (JustCard <$> card), cardViewState: Default } Nothing indexFilter
    -- _ -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } Nothing indexFilter

  where
    addEntryToIndex entry = do
      let newIndex = Index (entry : list)
      manageUpdateIndex newIndex { cardView: (CardFromReference entry), cardViewState: Default }
    
    removeReferenceFromIndex (CardEntry { cardReference: reference }) = do
      let newIndex = Index (filter (\(CardEntry { cardReference }) -> cardReference /= reference) list)
      manageUpdateIndex newIndex { cardView: NoCard, cardViewState: Default }

    updateReferenceInIndex (CardEntry { cardReference: reference }) entry = do --TODO finish implementation based on card versioning
      let newIndex = Index (entry : filter (\(CardEntry { cardReference }) -> cardReference /= reference) list)
      manageUpdateIndex newIndex { cardView: (CardFromReference entry), cardViewState: Default }

    manageUpdateIndex :: Index -> CardViewState -> Aff (ComplexIndexFilter -> CardsViewResult)
    manageUpdateIndex newIndex cardViewState = do
      updateResult <- liftAff $ runExceptT $ updateIndex newIndex
      case updateResult of
        Right _   -> pure $ OpResult newIndex cardViewState Nothing
        Left  err -> 
          case err of 
            CannotInitState       _                           -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            InvalidOperationError _                           -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a wrongly programmed operation
            InvalidStateError    (CorruptedState           _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            InvalidStateError    (MissingValue             _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            InvalidStateError    (CorruptedSavedPassphrase _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            ProtocolError        (RequestError             _) -> manageUpdateIndex newIndex cardViewState -- Retry at infinitum?
            ProtocolError        (ResponseError            i) -> -- Check values
              case i of
                404 -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Something is really wrong with the server
                500 -> manageUpdateIndex newIndex cardViewState -- Retry at infinitum hoping the server gets better?
                _   -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Well...
            ProtocolError        (SRPError                 _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Shouldn't happen, restart
            ProtocolError        (DecodeError              _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Wrong data has been saved on the server
            ProtocolError        (CryptoError              _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Corrupted data on server
            ProtocolError        (IllegalRequest           _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- The app is not working well
            ProtocolError        (IllegalResponse          _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- The server did something wrong, but the operation should have worked
            ImportError           _                           -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)

getUpdateIndexInfo :: ProxyConnectionStatus -> CardsViewInfo -> IndexUpdateData -> Maybe AppError -> CardsViewInfo
getUpdateIndexInfo _ info (IndexUpdateData action card) err = 
  case action of 
    AddReference                 _ -> info { error = err, indexFilter   = { archived: false, indexFilter: NoFilter }, cardViewState = { cardView: (CardForm $ NewCard card), cardViewState: Loading } }
    CloneReference               _ -> info { error = err, cardViewState = { cardView: (maybe NoCard JustCard card), cardViewState: Loading } }
    DeleteReference              _ -> info { error = err, cardViewState = { cardView: (maybe NoCard JustCard card), cardViewState: Loading } }
    ChangeReferenceWithEdit    _ _ -> info { error = err, cardViewState = { cardView: (CardForm $ NewCard card),    cardViewState: Loading } }
    ChangeReferenceWithoutEdit _ _ -> info { error = err, cardViewState = { cardView: (maybe NoCard JustCard card), cardViewState: Loading } }
    _                              -> info { error = err, cardViewState = { cardView: NoCard                      , cardViewState: Default } }
