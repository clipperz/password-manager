module OperationalWidgets.CardsManagerWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div)
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Index (CardReference, Index)
import Effect.Class.Console (log)
import Views.IndexView (indexView)
import OperationalWidgets.CardWidget (cardWidget, IndexUpdateAction)

data CardsViewAction = UpdateIndex IndexUpdateAction | ShowCard CardReference
instance showCardsViewAction :: Show CardsViewAction where
  show (UpdateIndex a) = "UpdateIndex " <> show a
  show (ShowCard ref) = "Show Card " <> show ref

cardsManagerWidget :: forall a. Index -> Maybe CardReference -> Widget HTML a
cardsManagerWidget index mc = do
  res <- case mc of
    Nothing -> div [] [ ShowCard <$> indexView index ]
    Just c -> div [] [
        ShowCard <$> indexView index
      , UpdateIndex <$> cardWidget c
    ]
  case res of
    UpdateIndex action -> do
      _ <- log $ show action
      cardsManagerWidget index mc
    ShowCard ref -> cardsManagerWidget index (Just ref)
