module Views.CardsManagerView where


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
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unfoldable (fromMaybe)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Index (CardReference, Index(..), CardEntry(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class.Console (log)
import Functions.Communication.Cards (updateIndex)
import Functions.SRP as SRP
import Views.IndexView (indexView)
import Views.SimpleWebComponents (simpleButton)
import OperationalWidgets.CardWidget (cardWidget, IndexUpdateAction(..), createCardWidget)

data CardViewAction = UpdateIndex IndexUpdateAction | ShowCard CardReference | AddCard
instance showCardViewAction :: Show CardViewAction where
  show (UpdateIndex a) = "UpdateIndex " <> show a
  show (ShowCard ref)  = "Show Card " <> show ref
  show  AddCard        = "Add Card"

data CardView = NoCard | JustCard CardReference | CardForm Card

cardsManagerView :: Index -> CardView -> Maybe AppError -> Widget HTML CardViewAction
cardsManagerView i cv error = 
  let errorWidgets = (text <$> (fromMaybe (show <$> error))) :: Array (Widget HTML CardViewAction)
  in case cv of
    NoCard -> div [] $ errorWidgets <> [
      ShowCard <$> indexView i
      , simpleButton "Add card" false AddCard 
    ]
    JustCard c -> div [] $ errorWidgets <> [
      ShowCard <$> indexView i
      , UpdateIndex <$> cardWidget c
      , simpleButton "Add card" false AddCard 
    ]
    CardForm card -> div [] $ errorWidgets <> [
      ShowCard <$> indexView i
      , UpdateIndex <$> createCardWidget card
    ]
