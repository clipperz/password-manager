module DataModel.WidgetOperations where

import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Card (Card)
import DataModel.Index (CardReference, CardEntry)

data IndexUpdateData = IndexUpdateData IndexUpdateAction Card
instance showIndexUpdateData :: Show IndexUpdateData where
  show (IndexUpdateData action card) = "Do " <> show action <> " while showing " <> show card

data IndexUpdateAction = AddReference CardEntry
                       | CloneReference CardEntry 
                       | DeleteReference CardReference
                       | ChangeToReference CardReference CardEntry
                       | NoUpdate

instance showIndexUpdateAction :: Show IndexUpdateAction where
  show (AddReference c ) = "Add reference to " <> show c
  show (CloneReference c ) = "Clone reference to " <> show c
  show (DeleteReference c ) = "Delete reference to " <> show c
  show (ChangeToReference c c') = "Change reference of " <> show c <> " to " <> show c'
  -- show (ChangeToReference c ) = "Change reference of " <> show c 
  show NoUpdate = "No update"
