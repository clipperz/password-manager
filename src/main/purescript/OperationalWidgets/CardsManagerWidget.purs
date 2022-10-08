module OperationalWidgets.CardsManagerWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Show (show)
import DataModel.AppState (AppError)
import DataModel.Card (emptyCard)
import DataModel.Index (Index(..), CardEntry(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Functions.Communication.Cards (updateIndex)
import Functions.SRP as SRP
import Views.CardsManagerView (cardsManagerView, CardView(..), CardViewAction(..))
import OperationalWidgets.CardWidget (IndexUpdateAction(..))

data CardsViewResult = CardsViewResult CardViewAction | OpResult Index CardView (Maybe AppError)

cardsManagerWidget :: forall a. SRP.SRPConf -> Index -> CardView -> Widget HTML a
cardsManagerWidget conf ind mc = go ind (cardsManagerView ind mc) Nothing Nothing

  where
    go :: Index -> (Maybe AppError -> Widget HTML CardViewAction) -> Maybe AppError -> Maybe (Aff CardsViewResult) -> Widget HTML a
    go index view mError operation = do
      res <- case operation of
        Nothing -> CardsViewResult <$> (view mError)
        Just op -> (CardsViewResult <$> (view mError)) <|> (liftAff $ op)
      case res of
        CardsViewResult cva -> case cva of 
          UpdateIndex action -> do
            _ <- log $ show action
            go index (getUpdateIndexView index action) Nothing (Just (getUpdateIndexOp conf index action))
          AddCard -> go index (cardsManagerView index (CardForm emptyCard)) Nothing Nothing
          ShowCard ref -> go index (cardsManagerView index (JustCard ref)) Nothing Nothing
        OpResult i cv e -> go index (cardsManagerView i cv) e Nothing

getUpdateIndexOp :: SRP.SRPConf -> Index -> IndexUpdateAction -> Aff CardsViewResult
getUpdateIndexOp conf index@(Index_v1 list) action = 
  case action of 
    AddReference _ entry@(CardEntry_v1 { title: _, cardReference, archived: _, tags: _}) -> do
      let newIndex = Index_v1 (entry : list)
      updateResult <- liftAff $ runExceptT $ updateIndex conf newIndex
      case updateResult of
        Right _ -> pure $ OpResult newIndex (JustCard cardReference) Nothing
        Left err -> pure $ OpResult index NoCard (Just err) 
    _ -> pure $ OpResult index NoCard Nothing

getUpdateIndexView :: Index -> IndexUpdateAction -> (Maybe AppError -> Widget HTML CardViewAction)
getUpdateIndexView index action = 
  case action of 
    AddReference card _ -> cardsManagerView index (CardForm card)
    _ -> cardsManagerView index NoCard
