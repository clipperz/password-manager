module Views.CardsManagerView where


import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
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
  let disableIndex = case cv of
                      NoCard     -> false
                      JustCard _ -> false
                      CardForm _ -> true
  in  
    div [Props._id "cardsManager"] $ (text <$> (fromMaybe $ show <$> error)) <> [
      div [Props._id "indexView"] [
        ShowCard <$> indexView disableIndex i
      , simpleButton "Add card" disableIndex ShowAddCard 
      ]
    , div [Props._id "cardView"] $
      case cv of
        NoCard        -> []
        JustCard ref  -> [UpdateIndex <$> cardWidget ref]
        CardForm card -> [UpdateIndex <$> createCardWidget card]
    ]
