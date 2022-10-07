module OperationalWidgets.CardsManagerWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unfoldable (fromMaybe)
import DataModel.AppState (AppError)
import DataModel.Index (CardReference, Index(..), CardEntry(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Functions.Communication.Cards (updateIndex)
import Functions.SRP as SRP
import Views.IndexView (indexView)
import Views.SimpleWebComponents (simpleButton)
import OperationalWidgets.CardWidget (cardWidget, IndexUpdateAction(..), createCardWidget)

data CardsViewAction = UpdateIndex IndexUpdateAction | ShowCard CardReference | AddCard
instance showCardsViewAction :: Show CardsViewAction where
  show (UpdateIndex a) = "UpdateIndex " <> show a
  show (ShowCard ref)  = "Show Card " <> show ref
  show  AddCard        = "Add Card"

data CardView = NoCard | JustCard CardReference | CardForm

data CardsViewResult = CardsViewResult CardsViewAction | OpResult Index CardView (Maybe AppError)

cardsManagerWidget :: forall a. SRP.SRPConf -> Index -> CardView -> Widget HTML a
cardsManagerWidget conf index@(Index_v1 list) mc = go index (cardsManagerView index mc) Nothing Nothing

  where
    cardsManagerView :: Index -> CardView -> Array (Widget HTML a) -> Widget HTML CardsViewAction
    cardsManagerView i cv errorView = case cv of -- TODO: add error view
      NoCard -> div [] [
        ShowCard <$> indexView i
        , simpleButton "Add card" false AddCard 
      ]
      JustCard c -> div [] [
        ShowCard <$> indexView i
        , UpdateIndex <$> cardWidget c
        , simpleButton "Add card" false AddCard 
      ]
      CardForm -> div [] [
        ShowCard <$> indexView i
        , UpdateIndex <$> createCardWidget
      ]
    go :: Index -> (Array (Widget HTML a) -> Widget HTML CardsViewAction) -> Maybe AppError -> Maybe (Aff CardsViewResult) -> Widget HTML a
    go index view me operation = do
      let errorView = fromMaybe ((text <<< show) <$> me)
      res <- case operation of
        Nothing -> CardsViewResult <$> (view errorView)
        Just op -> (CardsViewResult <$> (view errorView)) <|> (liftAff $ op)
      case res of
        CardsViewResult cva -> case cva of 
          UpdateIndex action -> do
            _ <- log $ show action
            go index view Nothing (Just (getUpdateIndexOp conf index action))
          AddCard -> go index (cardsManagerView index CardForm) Nothing Nothing
          ShowCard ref -> go index (cardsManagerView index (JustCard ref)) Nothing Nothing
        OpResult i cv me -> go index (cardsManagerView i cv) me Nothing

getUpdateIndexOp :: SRP.SRPConf -> Index -> IndexUpdateAction -> Aff CardsViewResult
getUpdateIndexOp conf index@(Index_v1 list) action = 
  case action of 
    AddReference entry@(CardEntry_v1 { title, cardReference, archived, tags}) -> do
      let newIndex = Index_v1 (entry : list)
      updateResult <- liftAff $ runExceptT $ updateIndex conf newIndex
      case updateResult of
        Right _ -> pure $ OpResult newIndex (JustCard cardReference) Nothing
        Left err -> pure $ OpResult index NoCard (Just err) 
    _ -> pure $ OpResult index NoCard Nothing
