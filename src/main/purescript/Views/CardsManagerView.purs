module Views.CardsManagerView where


import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Index (CardReference, Index)
import DataModel.WidgetOperations (IndexUpdateAction(..))
import DataModel.WidgetState (WidgetState)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Views.IndexView (indexView)
import Views.SimpleWebComponents (simpleButton)
import OperationalWidgets.CardWidget (cardWidget)
import OperationalWidgets.CreateCardWidget (createCardWidget)

data CardViewAction = UpdateIndex IndexUpdateAction | ShowCard CardReference | ShowAddCard
instance showCardViewAction :: Show CardViewAction where
  show (UpdateIndex a) = "UpdateIndex " <> show a
  show (ShowCard ref)  = "Show Card " <> show ref
  show  ShowAddCard    = "Show Add Card"

type CardViewState = { cardView :: CardView, cardViewState :: WidgetState }

data CardView = NoCard | JustCard CardReference | CardForm Card
instance showCardView :: Show CardView where
  show NoCard = "NoCard"
  show (JustCard cr) = "JustCard " <> show cr
  show (CardForm c) = "CardForm " <> show c

cardsManagerView :: Index -> CardViewState -> Maybe AppError -> Widget HTML CardViewAction
cardsManagerView i { cardView: cv, cardViewState } error = 
  let disableIndex = case cv of
                      NoCard     -> false
                      JustCard _ -> false
                      CardForm _ -> true
      createWidget card = do
        res <- createCardWidget card cardViewState
        case res of
          Nothing -> pure $ ShowAddCard
          Just (Tuple newCard newEntry) -> pure $ UpdateIndex $ AddReference newCard newEntry -- TODO: debatable using AddReference as a default
  in do 
    res <- div [Props._id "cardsManager"] $ (text <$> (fromMaybe $ show <$> error)) <> [
      div [Props._id "indexView"] [
        ShowCard <$> indexView disableIndex i
      , simpleButton "Add card" disableIndex ShowAddCard 
      ]
    , div [Props._id "cardView"] $
      case cv of
        NoCard        -> []
        JustCard ref  -> [UpdateIndex <$> cardWidget ref cardViewState]
        CardForm card -> [createWidget card]
    ]
    case res of
      ShowCard ref -> cardsManagerView i { cardView: JustCard ref, cardViewState } Nothing -- TODO: discuss
      _ -> pure res

