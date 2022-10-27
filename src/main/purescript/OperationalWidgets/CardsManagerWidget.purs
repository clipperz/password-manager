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
import Data.Functor ((<$>))
import Data.List ((:), filter)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Card (emptyCard)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..), CardEntry(..))
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Functions.Communication.Cards (updateIndex)
import Views.CardsManagerView (cardsManagerView, CardView(..), CardViewAction(..), CardViewState)

data CardsViewResult = CardsViewResult CardViewAction | OpResult Index CardViewState (Maybe AppError)

cardsManagerWidget :: forall a. Index -> CardViewState -> Widget HTML a
cardsManagerWidget ind cardViewState = go ind (cardsManagerView ind cardViewState) Nothing Nothing

  where
    go :: Index -> (Maybe AppError -> Widget HTML CardViewAction) -> Maybe AppError -> Maybe (Aff CardsViewResult) -> Widget HTML a
    go index view mError operation = do
      res <- case operation of
        Nothing -> CardsViewResult <$> (view mError)
        Just op -> (CardsViewResult <$> (view mError)) <|> (liftAff $ op)
      case res of
        CardsViewResult cva -> case cva of 
          UpdateIndex updateData -> do
            _ <- log $ show updateData
            go index (getUpdateIndexView index updateData) Nothing (Just (getUpdateIndexOp index updateData))
          ShowAddCard -> go index (cardsManagerView index {cardView: (CardForm emptyCard), cardViewState: Default}) Nothing Nothing
          ShowCard ref -> go index (cardsManagerView index {cardView: (CardFromReference ref), cardViewState: Default}) Nothing Nothing
        OpResult i cv e -> go i (cardsManagerView i cv) e Nothing

getUpdateIndexOp :: Index -> IndexUpdateData -> Aff CardsViewResult
getUpdateIndexOp index@(Index list) (IndexUpdateData action _) =
  case action of 
    AddReference                        entry -> addEntryToIndex entry 
    CloneReference                      entry -> addEntryToIndex entry
    ChangeReferenceWithEdit    oldEntry entry -> updateReferenceInIndex oldEntry entry
    ChangeReferenceWithoutEdit oldEntry entry -> updateReferenceInIndex oldEntry entry
    DeleteReference            oldEntry       -> removeReferenceFromIndex oldEntry
    _ -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } Nothing

  where
    addEntryToIndex entry = do
      let newIndex = Index (entry : list)
      manageUpdateIndex newIndex { cardView: (CardFromReference entry), cardViewState: Default }
    
    removeReferenceFromIndex (CardEntry_v1 { cardReference: reference }) = do
      let newIndex = Index (filter (\(CardEntry_v1 { cardReference }) -> cardReference /= reference) list)
      manageUpdateIndex newIndex { cardView: NoCard, cardViewState: Default }

    updateReferenceInIndex (CardEntry_v1 { cardReference: reference }) entry = do --TODO finish implementation based on card versioning
      let newIndex = Index (entry : filter (\(CardEntry_v1 { cardReference }) -> cardReference /= reference) list)
      manageUpdateIndex newIndex { cardView: (CardFromReference entry), cardViewState: Default }

    manageUpdateIndex :: Index -> CardViewState -> Aff CardsViewResult
    manageUpdateIndex newIndex cardViewState = do
      updateResult <- liftAff $ runExceptT $ updateIndex newIndex
      case updateResult of
        Right _   -> pure $ OpResult newIndex cardViewState Nothing
        Left  err -> 
          case err of 
            InvalidStateError (CorruptedState  _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            InvalidStateError (MissingValue    _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- No solution to a corrupted state if not restarting the app
            ProtocolError     (RequestError    _) -> manageUpdateIndex newIndex cardViewState -- Retry at infinitum?
            ProtocolError     (ResponseError   i) -> -- Check values
              case i of
                404 -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Something is really wrong with the server
                500 -> manageUpdateIndex newIndex cardViewState -- Retry at infinitum hoping the server gets better?
                _   -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Well...
            ProtocolError     (SRPError        _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Shouldn't happen, restart
            ProtocolError     (DecodeError     _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Wrong data has been saved on the server
            ProtocolError     (CryptoError     _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- Corrupted data on server
            ProtocolError     (IllegalRequest  _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- The app is not working well
            ProtocolError     (IllegalResponse _) -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err) -- The server did something wrong, but the operation should have worked
            ImportError        _                  -> pure $ OpResult index { cardView: NoCard, cardViewState: Default } (Just err)

getUpdateIndexView :: Index -> IndexUpdateData -> (Maybe AppError -> Widget HTML CardViewAction)
getUpdateIndexView index (IndexUpdateData action card) = 
  case action of 
    AddReference                 _ -> cardsManagerView index { cardView: (CardForm card), cardViewState: Loading } 
    CloneReference               _ -> cardsManagerView index { cardView: (JustCard card), cardViewState: Loading }
    DeleteReference              _ -> cardsManagerView index { cardView: (JustCard card), cardViewState: Loading }
    ChangeReferenceWithEdit    _ _ -> cardsManagerView index { cardView: (CardForm card), cardViewState: Loading } 
    ChangeReferenceWithoutEdit _ _ -> cardsManagerView index { cardView: (JustCard card), cardViewState: Loading } 
    _                              -> cardsManagerView index { cardView:  NoCard,         cardViewState: Default }
