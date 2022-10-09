module Views.CardsManagerView where


import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unfoldable (fromMaybe)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Index (CardReference, Index)
import Views.IndexView (indexView)
import Views.SimpleWebComponents (simpleButton)
import OperationalWidgets.CardWidget (cardWidget, IndexUpdateAction, createCardWidget)

data CardViewAction = UpdateIndex IndexUpdateAction | ShowCard CardReference | ShowAddCard
instance showCardViewAction :: Show CardViewAction where
  show (UpdateIndex a) = "UpdateIndex " <> show a
  show (ShowCard ref)  = "Show Card " <> show ref
  show  ShowAddCard    = "Show Add Card"

data CardView = NoCard | JustCard CardReference | CardForm Card

cardsManagerView :: Index -> CardView -> Maybe AppError -> Widget HTML CardViewAction
cardsManagerView i cv error = 
  let errorWidgets = (text <$> (fromMaybe (show <$> error))) :: Array (Widget HTML CardViewAction)
  in case cv of
    NoCard -> div [] $ errorWidgets <> [
      ShowCard <$> indexView i
    , simpleButton "Add card" false ShowAddCard 
    ]
    JustCard ref -> div [] $ errorWidgets <> [
      ShowCard <$> indexView i
    , UpdateIndex <$> cardWidget ref
    , simpleButton "Add card" false ShowAddCard 
    ]
    CardForm card -> div [] $ errorWidgets <> [
      ShowCard <$> indexView i
    , UpdateIndex <$> createCardWidget card
    ]
