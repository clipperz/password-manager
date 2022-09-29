module Widgets.Index where

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
import Widgets.SimpleWebComponents (clickableListItemWidget)

data IndexUpdateAction = AddReference Card | DeleteReference Card | ChangeToReference Card Card | NoUpdate
instance showIndexUpdateAction :: Show IndexUpdateAction where
  show (AddReference c) = "Add reference to " <> show c
  show (DeleteReference c) = "Delete reference to " <> show c
  show (ChangeToReference c c') = "Change reference of " <> show c <> " to " <> show c'
  show NoUpdate = "No update"

indexCard :: Index -> Widget HTML CardReference
indexCard (Index_v1 cards) = do
  let sortedCards = fromFoldable $ sort cards :: Array CardEntry
  ol' $ (\(CardEntry_v1 { title, cardReference }) -> clickableListItemWidget (text title) cardReference) <$> sortedCards
