module DataModel.WidgetOperations where

import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.CardVersions.Card (Card)
import DataModel.IndexVersions.Index (CardEntry)

data IndexUpdateData = IndexUpdateData IndexUpdateAction (Maybe Card)
instance showIndexUpdateData :: Show IndexUpdateData where
  show (IndexUpdateData action card) = "Do " <> show action <> " while showing " <> show card

data IndexUpdateAction = AddReference CardEntry
                       | CloneReference CardEntry 
                       | DeleteReference CardEntry
                       | ChangeReferenceWithEdit CardEntry CardEntry
                       | ChangeReferenceWithoutEdit CardEntry CardEntry
                       | NoUpdateNecessary CardEntry
                       | NoUpdate

instance showIndexUpdateAction :: Show IndexUpdateAction where
  show (AddReference c ) = "Add reference to " <> show c
  show (CloneReference c ) = "Clone reference to " <> show c
  show (DeleteReference c ) = "Delete reference to " <> show c
  show (ChangeReferenceWithEdit c c') = "Change reference with edit of " <> show c <> " to " <> show c'
  show (ChangeReferenceWithoutEdit c c') = "Change reference without edit of " <> show c <> " to " <> show c'
  show (NoUpdateNecessary c) = "No update from act on " <> show c
  show (NoUpdate) = "No update"
