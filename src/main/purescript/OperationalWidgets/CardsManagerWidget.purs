module OperationalWidgets.CardsManagerWidget
  ( cardsManagerWidget
  , CardsManagerAction(..)
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Alternative ((*>))
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
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.AppState (AppError(..), InvalidStateError(..), ProxyConnectionStatus)
import DataModel.Card (Card)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..), CardEntry(..))
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Functions.Communication.Users (updateIndex)
import OperationalWidgets.CreateCardWidget (CardFormInput(..))
import Views.CardsManagerView (cardsManagerView, CardView(..), CardViewAction(..), CardViewState, CardsViewInfo, FilterViewStatus(..))
import Views.IndexView (IndexFilter(..), ComplexIndexFilter, addLastCardFilterInOr, removeAllLastCardFilter)

data CardsManagerAction = OpenUserArea

data CardsViewResult = CardsViewResult (Tuple CardsViewInfo CardViewAction) | OpResult Index CardViewState (Maybe AppError) ComplexIndexFilter

cardsManagerWidget :: ProxyConnectionStatus -> Index -> Maybe Card -> CardViewState -> Widget HTML CardsManagerAction
cardsManagerWidget proxyConnectionStatus ind card _ = do
  cardView <- pure $ case card of
      Just c  -> CardForm (NewCard $ Just c)
      Nothing -> NoCard
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
            CannotInitState       _                           -> defaultErrorHandling "No solution to a corrupted state if not restarting the app" err
            InvalidOperationError _                           -> defaultErrorHandling "No solution to a wrongly programmed operation"              err
            InvalidStateError    (CorruptedState           _) -> defaultErrorHandling "No solution to a corrupted state if not restarting the app" err
            InvalidStateError    (MissingValue             _) -> defaultErrorHandling "No solution to a corrupted state if not restarting the app" err
            InvalidStateError    (CorruptedSavedPassphrase _) -> defaultErrorHandling "No solution to a corrupted state if not restarting the app" err
            ProtocolError        (RequestError             _) -> manageUpdateIndex newIndex cardViewState -- Retry at infinitum?
            ProtocolError        (ResponseError            i) -> -- Check values
              case i of
                404 -> defaultErrorHandling "Something is really wrong with the server" err
                500 -> manageUpdateIndex newIndex cardViewState -- Retry at infinitum hoping the server gets better?
                _   -> defaultErrorHandling "Well..." err
            ProtocolError        (SRPError                 _) -> defaultErrorHandling "Shouldn't happen, restart" err
            ProtocolError        (DecodeError              _) -> defaultErrorHandling "Wrong data has been saved on the server" err
            ProtocolError        (CryptoError              _) -> defaultErrorHandling "Corrupted data on server" err
            ProtocolError        (IllegalRequest           _) -> defaultErrorHandling "The app is not working well" err
            ProtocolError        (IllegalResponse          _) -> defaultErrorHandling "The server did something wrong, but the operation should have worked" err
            ImportError           _                           -> defaultErrorHandling "TODO" err
            UnhandledCondition    _                           -> defaultErrorHandling "TODO" err
            InvalidVersioning     _ _                         -> defaultErrorHandling "TODO" err
    
     where
        defaultErrorHandling :: String -> AppError -> Aff (ComplexIndexFilter -> CardsViewResult)
        defaultErrorHandling msg err = (logMessage msg err) *> (pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err))

        logMessage :: String -> AppError -> Aff Unit
        logMessage msg err = (log $ "[" <> (show err) <> "] - " <> msg)

getUpdateIndexInfo :: ProxyConnectionStatus -> CardsViewInfo -> IndexUpdateData -> Maybe AppError -> CardsViewInfo
getUpdateIndexInfo _ info (IndexUpdateData action card) err = 
  case action of 
    AddReference                 _ -> info { error = err, indexFilter   = { archived: false, indexFilter: NoFilter }, cardViewState = { cardView: (CardForm $ NewCard card), cardViewState: Loading } }
    CloneReference               _ -> info { error = err, cardViewState = { cardView: (maybe NoCard JustCard card), cardViewState: Loading } }
    DeleteReference              _ -> info { error = err, cardViewState = { cardView: (maybe NoCard JustCard card), cardViewState: Loading } }
    ChangeReferenceWithEdit    _ _ -> info { error = err, cardViewState = { cardView: (CardForm $ NewCard card),    cardViewState: Loading } }
    ChangeReferenceWithoutEdit _ _ -> info { error = err, cardViewState = { cardView: (maybe NoCard JustCard card), cardViewState: Loading } }
    _                              -> info { error = err, cardViewState = { cardView: NoCard                      , cardViewState: Default } }
