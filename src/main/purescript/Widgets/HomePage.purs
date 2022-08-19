module Widgets.HomePage where

import Concur.Core (Widget)
import Concur.Core.FRP (loopW, dyn)
import Concur.React (HTML)
import Concur.React.DOM (div)
import Control.Bind (discard)
import Control.Semigroupoid ((<<<))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Map (fromFoldable, lookup, Map)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import DataModel.IndexCard (CardIndex(..), CardReference(..), IndexCard(..))
import DataModel.Card (CardField(..), Card, CardValues)
import Widgets.Cards (card, CardAction)
import Widgets.IndexCard (indexCard)

card0 :: CardValues
card0 = { title: "Unibo", tags: ["uni", "stress"], fields: [(CardField_v1  {name: "username", value: "ciao", locked: false}), (CardField_v1 {name: "password", value: "ciaooo", locked: true})], notes: "oh my notes"}

card1 :: CardValues
card1 = { title: "Bank", tags: ["bank"], fields: [(CardField_v1 {name: "username", value: "bank_username", locked: false}), (CardField_v1 {name: "password", value: "bank_password", locked: true})], notes: "this is a bank"}

cards :: Map String Card
cards = fromFoldable [Tuple "unibo" {reference: "unibo", content: card0}, Tuple "bank" {reference: "bank", content: card1}]

cardIndex :: IndexCard
cardIndex = IndexCard_v1 {cards: fromFoldable [
  Tuple "unibo" (CardIndex_v1 { title: "Unibo", cardReference: CardReference_v1 {reference: "unibo", key: "TODO"}, archived: false}),
  Tuple "bank"  (CardIndex_v1 { title: "Bank",  cardReference: CardReference_v1 {reference: "bank",  key: "TODO"}, archived: false})
]}

data CardsViewAction = ShowCard (Maybe CardReference) | ActOnCard CardAction
instance showCardsViewAction :: Show CardsViewAction where
  show (ShowCard cr) = "ShowCard " <> show cr
  show (ActOnCard ca) = "CardAction " <> (show ca)

cardsView :: IndexCard -> Maybe Card -> Widget HTML CardsViewAction
cardsView index Nothing = div [] [(ShowCard <<< Just) <$> indexCard index]
cardsView index (Just c) = div [] [
    (ShowCard <<< Just) <$> indexCard index
  , ActOnCard <$> card c
]

homePage :: Widget HTML CardsViewAction
homePage = dyn $ loopW (ShowCard Nothing) (\cva ->
  case cva of
    ShowCard Nothing -> cardsView cardIndex Nothing
    ShowCard (Just (CardReference_v1 {reference: ref, key: _})) -> cardsView cardIndex $ lookup ref cards
    ActOnCard a -> do
      liftEffect $ log $ show a
      cardsView cardIndex Nothing
)