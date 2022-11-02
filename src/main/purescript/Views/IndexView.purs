module Views.IndexView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (ol, text)
import Data.Array (fromFoldable, elem)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (sort, filter)
import Data.Show (class Show, show)
import Data.Semigroup ((<>))
import Data.String (contains)
import Data.String.Common (toLower)
import Data.String.Pattern (Pattern(..))
import DataModel.Index (Index(..), CardEntry(..))
import Views.SimpleWebComponents (clickableListItemWidget)

data IndexFilter = TitleFilter String | TagFilter String | NoFilter
instance showIndexFilter :: Show IndexFilter where
  show (TitleFilter title) = "Title filter: " <> title
  show (TagFilter tag) = "Tag filter: " <> tag
  show (NoFilter) = "No filter"

indexView :: Index -> IndexFilter -> Widget HTML CardEntry
indexView (Index cards) indexFilter = do
  let sortedCards = fromFoldable $ filter (toFilterFunc indexFilter) $ sort cards :: Array CardEntry
  ol []
    ((\entry@(CardEntry { title, archived }) -> 
      clickableListItemWidget false (text title) (if archived then ["archived"] else []) entry
     ) <$> sortedCards)

  where
    toFilterFunc (TitleFilter title) = \(CardEntry r) -> if title == "" then true else contains (Pattern (toLower title)) (toLower r.title)
    toFilterFunc (TagFilter tag)     = \(CardEntry r) -> elem tag r.tags
    toFilterFunc  NoFilter           = \_ -> true
