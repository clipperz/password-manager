module Views.IndexView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (ol, text)
import Control.Semigroupoid ((<<<))
import Data.Array (fromFoldable, elem, null, (:))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Eq ((==), class Eq)
import Data.Foldable (any, fold, foldr)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&), (||), not)
import Data.List (sort, filter, List(..))
import Data.Show (class Show, show)
import Data.Semigroup ((<>))
import Data.String (contains)
import Data.String.Common (toLower)
import Data.String.Pattern (Pattern(..))
import DataModel.Index (Index(..), CardEntry(..))
import Views.SimpleWebComponents (clickableListItemWidget)

data IndexFilter = ComposedAndFilter IndexFilter IndexFilter | ComposedOrFilter IndexFilter IndexFilter | GeneralFilter String | TagFilter String | SpecificCardFilter CardEntry | RecentFilter | UntaggedFilter | NoFilter
instance showIndexFilter :: Show IndexFilter where
  show (ComposedAndFilter f f') = "Composed and filter: " <> show f <> " and " <> show f'
  show (ComposedOrFilter f f') = "Composed or filter: " <> show f <> " or " <> show f'
  show (GeneralFilter query) = "General filter: " <> query
  show (SpecificCardFilter (CardEntry r)) = "Specific card filter: " <> r.title
  show (TagFilter tag) = "Tag filter: " <> tag
  show (RecentFilter) = "Recent filter"
  show (UntaggedFilter) = "Untagged"
  show (NoFilter) = "No filter"

derive instance eqIndexFilter :: Eq IndexFilter

type ComplexIndexFilter = { archived :: Boolean, indexFilter :: IndexFilter }

indexView :: Index -> ComplexIndexFilter -> Widget HTML CardEntry
indexView (Index cards) complexIndexFilter = do
  let sortedCards = fromFoldable $ filter (complexToFilterFunc complexIndexFilter) $ sort cards :: Array CardEntry
  ol []
    ((\entry@(CardEntry { title, archived }) -> 
      clickableListItemWidget false (text title) (if archived then ["archived"] else []) entry
     ) <$> sortedCards)

complexToFilterFunc :: ComplexIndexFilter -> (CardEntry -> Boolean)
complexToFilterFunc { archived, indexFilter } = \ce@(CardEntry r) -> ((archived) || (not archived && r.archived == false)) && (toFilterFunc indexFilter) ce

toFilterFunc :: IndexFilter -> (CardEntry -> Boolean)
toFilterFunc (ComposedOrFilter f f')  = \a -> (toFilterFunc f) a || (toFilterFunc f') a
toFilterFunc (ComposedAndFilter f f') = \a -> (toFilterFunc f) a && (toFilterFunc f') a
toFilterFunc (GeneralFilter query)   = \(CardEntry r) -> if query == "" then true else any (contains (Pattern (toLower query))) (toLower <$> (r.title : r.tags))
toFilterFunc (SpecificCardFilter ce) = \ce' -> ce == ce'
toFilterFunc (TagFilter tag)       = \(CardEntry r) -> elem tag r.tags
toFilterFunc (RecentFilter)        = \(CardEntry _) -> true --TODO
toFilterFunc (UntaggedFilter)      = \(CardEntry r) -> null r.tags
toFilterFunc  NoFilter             = \_ -> true
