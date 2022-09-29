module Widgets.HomePage where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, div')
import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HexString (HexString)
import Data.List (List(..))
import Data.Map (fromFoldable, Map)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Index (CardReference, Index(..))
import DataModel.Card (Card)
import Effect.Class.Console (log)
import Widgets.Cards (card, CardAction, cardWidget)
import Widgets.Index (indexCard, IndexUpdateAction)
import Widgets.SimpleWebComponents (simpleButton)

cards :: Map HexString Card
cards = fromFoldable []

cardIndex :: Index
cardIndex = Index_v1 Nil

data CardsViewAction = UpdateIndex IndexUpdateAction | ShowCard CardReference
instance showCardsViewAction :: Show CardsViewAction where
  show (UpdateIndex a) = "UpdateIndex " <> show a
  show (ShowCard ref) = "Show Card " <> show ref

cardsView :: Index -> Maybe CardReference -> Widget HTML CardsViewAction
cardsView index mc = do
  res <- case mc of
    Nothing -> div [] [ ShowCard <$> indexCard index ]
    Just c -> div [] [
        ShowCard <$> indexCard index
      , UpdateIndex <$> cardWidget c
    ]
  case res of
    UpdateIndex action -> do
      _ <- log $ show action
      cardsView index mc
    ShowCard ref -> cardsView index (Just ref)
        

data HomePageAction = DefaultHomePageAction | LogoutAction

homePage :: Index -> Maybe CardReference -> Widget HTML HomePageAction
homePage index cardReference = div [] [
  DefaultHomePageAction <$ cardsView index cardReference
, simpleButton "Logout" false LogoutAction
]
