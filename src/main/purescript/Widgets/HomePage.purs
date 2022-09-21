module Widgets.HomePage where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div)
import Control.Semigroupoid ((<<<))
import Data.Functor ((<$>))
import Data.HexString (HexString)
import Data.List (List(..))
import Data.Map (fromFoldable, Map)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Index (CardReference, Index(..))
import DataModel.Card (Card)
import Widgets.Cards (card, CardAction)
import Widgets.Index (indexCard)
import Widgets.SimpleWebComponents (simpleButton)

cards :: Map HexString Card
cards = fromFoldable []

cardIndex :: Index
cardIndex = Index_v1 Nil

data CardsViewAction = ShowCard (Maybe CardReference) | ActOnCard Card CardAction
instance showCardsViewAction :: Show CardsViewAction where
  show (ShowCard cr) = "ShowCard " <> show cr
  show (ActOnCard _ ca) = "CardAction " <> (show ca)

cardsView :: Index -> Maybe Card -> Widget HTML CardsViewAction
cardsView index Nothing = div [] [(ShowCard <<< Just) <$> indexCard index]
cardsView index (Just c) = div [] [
    (ShowCard <<< Just) <$> indexCard index
  , ActOnCard c <$> card c
]

data HomePageAction = CardsViewAction CardsViewAction | LogoutAction

homePage :: Index -> Maybe Card -> Widget HTML HomePageAction
homePage index card = div [] [
  CardsViewAction <$> cardsView index card
, simpleButton "Logout" false LogoutAction
]
  