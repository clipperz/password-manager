module OperationalWidgets.CardsManagerWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>), flap)
import Data.List ((:), filter)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Card (emptyCard)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..), CardEntry(..))
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Functions.Communication.Users (updateIndex)
import Views.CardsManagerView (cardsManagerView, CardView(..), CardViewAction(..), CardViewState)
import Views.IndexView (IndexFilter(..), ComplexIndexFilter)

data CardsViewResult = CardsViewResult (Tuple ComplexIndexFilter CardViewAction) | OpResult Index CardViewState (Maybe AppError) ComplexIndexFilter

cardsManagerWidget :: forall a. Index -> CardViewState -> Widget HTML a
cardsManagerWidget ind cardViewState = go ind { archived: false, indexFilter: NoFilter } (\f -> cardsManagerView ind f cardViewState) Nothing Nothing

  where
    go :: Index -> ComplexIndexFilter -> (ComplexIndexFilter -> Maybe AppError -> Widget HTML (Tuple ComplexIndexFilter CardViewAction)) -> Maybe AppError -> Maybe (Aff CardsViewResult) -> Widget HTML a
    go index indexFilter view mError operation = do
      res <- case operation of
        Nothing -> CardsViewResult <$> (view indexFilter mError)
        Just op -> (CardsViewResult <$> (view indexFilter mError)) <|> (liftAff $ op)
      case res of
        CardsViewResult (Tuple f cva) -> case cva of 
          UpdateIndex updateData -> do
            _ <- log $ show updateData
            go index f (\ff -> getUpdateIndexView index ff updateData) Nothing (Just (getUpdateIndexOp index f updateData))
          ShowAddCard -> go index f (\ff -> cardsManagerView index ff {cardView: (CardForm emptyCard), cardViewState: Default}) Nothing Nothing
          ShowCard ref -> go index f (\ff -> cardsManagerView index ff {cardView: (CardFromReference ref), cardViewState: Default}) Nothing Nothing
        OpResult i cv e f -> go i f (\ff -> cardsManagerView i ff cv) e Nothing

getUpdateIndexOp :: Index -> ComplexIndexFilter -> IndexUpdateData -> Aff CardsViewResult
getUpdateIndexOp index@(Index list) indexFilter (IndexUpdateData action _) =
  case action of 
    AddReference                        entry -> flap (addEntryToIndex entry) { archived: false, indexFilter: ComposedOrFilter (SpecificCardFilter entry) indexFilter.indexFilter } 
    CloneReference                      entry -> flap (addEntryToIndex entry) indexFilter
    ChangeReferenceWithEdit    oldEntry entry -> flap (updateReferenceInIndex oldEntry entry) indexFilter
    ChangeReferenceWithoutEdit oldEntry entry -> flap (updateReferenceInIndex oldEntry entry) indexFilter
    DeleteReference            oldEntry       -> flap (removeReferenceFromIndex oldEntry) indexFilter
    _ -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } Nothing indexFilter

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
            CannotInitState    _                           -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            InvalidStateError (CorruptedState           _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            InvalidStateError (MissingValue             _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            InvalidStateError (CorruptedSavedPassphrase _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            ProtocolError     (RequestError             _) -> manageUpdateIndex newIndex cardViewState -- Retry at infinitum?
            ProtocolError     (ResponseError            i) -> -- Check values
              case i of
                404 -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Something is really wrong with the server
                500 -> manageUpdateIndex newIndex cardViewState -- Retry at infinitum hoping the server gets better?
                _   -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Well...
            ProtocolError     (SRPError                 _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Shouldn't happen, restart
            ProtocolError     (DecodeError              _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Wrong data has been saved on the server
            ProtocolError     (CryptoError              _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Corrupted data on server
            ProtocolError     (IllegalRequest           _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- The app is not working well
            ProtocolError     (IllegalResponse          _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- The server did something wrong, but the operation should have worked
            ImportError        _                           -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)

getUpdateIndexView :: Index -> ComplexIndexFilter -> IndexUpdateData -> (Maybe AppError -> Widget HTML (Tuple ComplexIndexFilter CardViewAction))
getUpdateIndexView index indexFilter (IndexUpdateData action card) = 
  case action of 
    AddReference                 _ -> cardsManagerView index { archived: false, indexFilter: NoFilter } { cardView: (CardForm card), cardViewState: Loading } 
    CloneReference               _ -> cardsManagerView index indexFilter  { cardView: (JustCard card), cardViewState: Loading }
    DeleteReference              _ -> cardsManagerView index indexFilter  { cardView: (JustCard card), cardViewState: Loading }
    ChangeReferenceWithEdit    _ _ -> cardsManagerView index indexFilter  { cardView: (CardForm card), cardViewState: Loading } 
    ChangeReferenceWithoutEdit _ _ -> cardsManagerView index indexFilter  { cardView: (JustCard card), cardViewState: Loading } 
    _                              -> cardsManagerView index indexFilter  { cardView:  NoCard,         cardViewState: Default }
