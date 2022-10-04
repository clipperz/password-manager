module OperationalWidgets.CardsManagerWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div)
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Index (CardReference, Index(..), CardEntry(..))
import Effect.Class.Console (log)
import Views.IndexView (indexView)
import Views.SimpleWebComponents (simpleButton)
import OperationalWidgets.CardWidget (cardWidget, IndexUpdateAction(..), createCardWidget)

data CardsViewAction = UpdateIndex IndexUpdateAction | ShowCard CardReference | AddCard
instance showCardsViewAction :: Show CardsViewAction where
  show (UpdateIndex a) = "UpdateIndex " <> show a
  show (ShowCard ref)  = "Show Card " <> show ref
  show  AddCard        = "Add Card"

data CardView = NoCard | JustCard CardReference | CardForm

cardsManagerWidget :: forall a. Index -> CardView -> Widget HTML a
cardsManagerWidget index@(Index_v1 list) mc = do
  res <- case mc of
    NoCard -> div [] [ 
      ShowCard <$> indexView index
      , simpleButton "Add card" false AddCard 
    ]
    JustCard c -> div [] [
        ShowCard <$> indexView index
      , UpdateIndex <$> cardWidget c
      , simpleButton "Add card" false AddCard 
    ]
    CardForm -> div [] [
        ShowCard <$> indexView index
      , UpdateIndex <$> createCardWidget
    ]
  case res of
    UpdateIndex action -> do
      _ <- log $ show action
      case action of 
        AddReference entry@(CardEntry_v1 { title, cardReference, archived, tags}) -> do
          let newIndex = Index_v1 (entry : list)
          cardsManagerWidget newIndex (JustCard cardReference)
        _ -> cardsManagerWidget index mc
    ShowCard ref -> cardsManagerWidget index (JustCard ref)
    AddCard -> cardsManagerWidget index CardForm
