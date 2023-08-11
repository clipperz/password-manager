module Views.IndexView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (ol, text)
import Data.Array (fromFoldable, elem, null, (:))
import Data.Eq ((==), (/=), class Eq)
import Data.Foldable (any)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&), (||), not)
import Data.List (sort, nub, filter, List, takeEnd, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((>=))
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

type ContextualFilterInfo = { allLastUses :: List Number }

indexView :: Index -> Maybe CardEntry -> ComplexIndexFilter -> Widget HTML CardEntry
indexView (Index cards) mCe complexIndexFilter = do
  let info = { allLastUses: (\(CardEntry r) -> r.lastUsed) <$> cards } 
  let sortedCards = fromFoldable $ filter (complexToFilterFunc info complexIndexFilter) $ sort cards :: Array CardEntry
  ol []
    ((\entry@(CardEntry { title, archived }) -> 
      case mCe of
        Nothing -> clickableListItemWidget false (text title) (if archived then ["archived"] else []) entry
        Just ce -> let selectedClass = if ce == entry then ["selected"] else [] 
                    in clickableListItemWidget false (text title) (selectedClass <> (if archived then ["archived"] else [])) entry
     ) <$> sortedCards)

complexToFilterFunc :: ContextualFilterInfo -> ComplexIndexFilter -> (CardEntry -> Boolean)
complexToFilterFunc info { archived, indexFilter } = 
  case indexFilter of
    SpecificCardFilter ce -> \ce' -> ce == ce'
    ComposedOrFilter (SpecificCardFilter ce) f -> \ce' -> (ce == ce') || ((complexToFilterFunc info {archived, indexFilter: f}) ce')
    _ -> \ce@(CardEntry r) -> ((archived) || (not archived && r.archived == false)) && (toFilterFunc info indexFilter) ce

toFilterFunc :: ContextualFilterInfo -> IndexFilter -> (CardEntry -> Boolean)
toFilterFunc info (ComposedOrFilter f f')  = \a -> (toFilterFunc info f) a || (toFilterFunc info f') a
toFilterFunc info (ComposedAndFilter f f') = \a -> (toFilterFunc info f) a && (toFilterFunc info f') a
toFilterFunc _    (GeneralFilter query)    = \(CardEntry r) -> if query == "" then true else any (contains (Pattern (toLower query))) (toLower <$> (r.title : r.tags))
toFilterFunc _    (SpecificCardFilter ce)  = \ce' -> ce == ce'
toFilterFunc _    (TagFilter tag)          = \(CardEntry r) -> elem tag r.tags
toFilterFunc info  RecentFilter            = \(CardEntry r) -> r.lastUsed >= (computeTimestampOfLastNUses 10 info.allLastUses)
toFilterFunc _    (UntaggedFilter)         = \(CardEntry r) -> null r.tags
toFilterFunc _     NoFilter                = \_ -> true

computeTimestampOfLastNUses :: Int -> List Number -> Number
computeTimestampOfLastNUses n ts = fromMaybe 0.0 $ head $ takeEnd n $ sort $ nub $ filter ((/=) 0.0) ts

removeLastCardFilter :: ComplexIndexFilter -> Maybe CardEntry -> ComplexIndexFilter
removeLastCardFilter cf@{ archived, indexFilter } mRef =
  case indexFilter of
    ComposedAndFilter (SpecificCardFilter ce) filter -> if (Just ce) == mRef then cf else{ archived, indexFilter: filter }
    ComposedAndFilter filter (SpecificCardFilter ce) -> if (Just ce) == mRef then cf else{ archived, indexFilter: filter }
    ComposedOrFilter (SpecificCardFilter ce) filter -> if (Just ce) == mRef then cf else{ archived, indexFilter: filter }
    ComposedOrFilter filter (SpecificCardFilter ce) -> if (Just ce) == mRef then cf else{ archived, indexFilter: filter }
    SpecificCardFilter ce -> if (Just ce) == mRef then cf else { archived, indexFilter: NoFilter }
    _ -> cf

removeAllLastCardFilter :: ComplexIndexFilter -> ComplexIndexFilter
removeAllLastCardFilter cf@{ archived, indexFilter } =
  case indexFilter of
    ComposedAndFilter (SpecificCardFilter _) filter -> { archived, indexFilter: filter }
    ComposedAndFilter filter (SpecificCardFilter _) -> { archived, indexFilter: filter }
    ComposedOrFilter (SpecificCardFilter _) filter  -> { archived, indexFilter: filter }
    ComposedOrFilter filter (SpecificCardFilter _)  -> { archived, indexFilter: filter }
    SpecificCardFilter _                            -> { archived, indexFilter: NoFilter }
    _ -> cf

addLastCardFilterInOr :: CardEntry -> ComplexIndexFilter -> ComplexIndexFilter
addLastCardFilterInOr entry { archived, indexFilter } = { archived, indexFilter: ComposedOrFilter (SpecificCardFilter entry) indexFilter }
