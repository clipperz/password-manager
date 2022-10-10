module DataModel.WidgetOperations where

import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Card (Card)
import DataModel.Index (CardReference, CardEntry)

data IndexUpdateAction = AddReference Card CardEntry 
                       | CloneReference CardEntry 
                       | DeleteReference CardReference 
                       | ChangeToReference Card CardReference CardEntry 
                       | NoUpdate
instance showIndexUpdateAction :: Show IndexUpdateAction where
  show (AddReference c _) = "Add reference to " <> show c
  show (CloneReference c ) = "Clone reference to " <> show c
  show (DeleteReference c) = "Delete reference to " <> show c
  show (ChangeToReference c c' c'') = "Change reference of " <> show c <> " to " <> show c''
  show NoUpdate = "No update"
