module Views.IndexView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (ol', text)
import Data.Array (fromFoldable)
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.List (sort)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Unit (Unit)

import DataModel.Card (Card(..))
import DataModel.Index (Index(..), CardEntry(..), CardReference)
import Views.SimpleWebComponents (clickableListItemWidget)

indexView :: Index -> Widget HTML CardReference
indexView (Index_v1 cards) = do
  let sortedCards = fromFoldable $ sort cards :: Array CardEntry
  ol' $ (\(CardEntry_v1 { title, cardReference }) -> clickableListItemWidget (text title) cardReference) <$> sortedCards
