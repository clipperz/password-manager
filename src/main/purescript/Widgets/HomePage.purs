module Widgets.HomePage where

import Concur.Core (Widget)
import Concur.Core.FRP (loopW, dyn)
import Concur.React (HTML)
import Concur.React.DOM (div)
import Control.Bind (discard)
import Control.Semigroupoid ((<<<))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString)
import Data.List (List(..), (:))
import Data.Map (fromFoldable, lookup, Map)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import DataModel.Index (CardEntry(..), CardReference(..), Index(..))
import DataModel.Card (CardField(..), Card(..), CardValues(..))
import Widgets.Cards (card, CardAction)
import Widgets.Index (indexCard)

cards :: Map HexString Card
cards = fromFoldable []

cardIndex :: Index
cardIndex = Index_v1 Nil

data CardsViewAction = ShowCard (Maybe CardReference) | ActOnCard CardAction
instance showCardsViewAction :: Show CardsViewAction where
  show (ShowCard cr) = "ShowCard " <> show cr
  show (ActOnCard ca) = "CardAction " <> (show ca)

cardsView :: Index -> Maybe Card -> Widget HTML CardsViewAction
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