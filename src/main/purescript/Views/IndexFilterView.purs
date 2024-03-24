module IndexFilterView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, form, input, int, label, li, ol, span, text)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>))
import Control.Category (identity, (>>>))
import Data.Array (any, nub, sort, (:))
import Data.Eq (class Eq, (==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra (not, (||))
import Data.List (List, fold, length)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord (compare, (<))
import Data.Semigroup ((<>))
import Data.Set (isEmpty, member, toUnfoldable)
import Data.String (Pattern(..), contains, toLower)
import DataModel.IndexVersions.Index (CardEntry(..), Index(..))

numberOfRecent :: Int
numberOfRecent =  10

data Filter = All | Recent | Untagged | Search String | Tag String

derive instance eqFilter :: Eq Filter

type FilterData = { 
  archived :: Boolean
, filter :: Filter
, filterViewStatus :: FilterViewStatus
, searchString :: String
}

data FilterViewStatus = FilterViewClosed | FilterViewOpen

getClassNameFromFilterStatus :: FilterViewStatus -> String
getClassNameFromFilterStatus status = case status of
  FilterViewClosed  -> "closed"
  FilterViewOpen    -> "open"

initialFilterData :: FilterData
initialFilterData = {
  archived: false
, filter:   All
, filterViewStatus: FilterViewClosed
, searchString: ""
}

indexFilterView :: FilterData -> Index -> Widget HTML FilterData
indexFilterView filterData@{archived, filter, searchString} (Index {entries}) = div [Props._id "filterView"] [
    (filterData {filterViewStatus = FilterViewClosed}) <$ div [Props.onClick, Props.className "mask"] []
  , div [Props.className "content"] [
      div [Props.className "filter"] [
        ol [Props.className "defaultSets"] [
          getFilterListElement All      "All"      ["allCards"]      (filter == All || filter == (Search ""))
        , getFilterListElement Recent   "Recent"   ["recentCards"]   (filter == Recent)
        , getFilterListElement Untagged "Untagged" ["untaggedCards"] (filter == Untagged)
        ] <#> updateFilter
      , form  [ Props._id "searchForm"
              , Props.classList [searchFormClassName]
              , Props.onSubmit $> filterData {filterViewStatus = FilterViewClosed}
              ] [
                label [Props.className "search"] [
                  span [Props.className "label"] [text "search"]
                , input [ Props._type "text"
                        , Props.placeholder "search"
                        , Props._id "searchInputField"
                        , Props.value searchString
                        , Props.disabled false
                        , Props.unsafeTargetValue <$> Props.onChange
                        , Props.unsafeTargetValue <$> Props.onFocus
                        ] <#> (\search -> filterData {filter = Search search, searchString = search})
                ]
              , span [Props.className "count"] [int $ filterCardsNumber (Search searchString)]
              ]
      , div [Props.className "tags"] [
          span [Props.className "tags"] [text "Tags"]
        , ol [Props._id "tagFilter"] (
                (\tag -> getFilterListElement (Tag tag) tag [] (filter == Tag tag)) 
            <$> (sort $ nub $ fold $ (\(CardEntry { tags }) -> toUnfoldable tags) 
            <$> (shownEntries entries Nothing archived))
          )
        ] <#> updateFilter
      ]
    , div [Props._id "archivedFilterArea"] [
        label [Props.className "showArchived"] [
          span [Props.className "label"] [text "Show archived cards"]
        , input [
            Props._type "checkbox"
          , Props.checked archived
          , Props.onChange
          ] $> (filterData { archived = not archived })
        ]
      , span [Props.className "count"] [int archivedCardsNumber]
      ]
    ]
  ]

  where
    searchFormClassName = case filter of
      Search "" -> Nothing
      Search _  -> Just "selected"
      _         -> Nothing

    archivedCardsNumber = length $ List.filter (\(CardEntry r) -> r.archived) entries

    updateFilter :: Filter -> FilterData
    updateFilter newFilter = filterData { filter = newFilter, filterViewStatus = FilterViewClosed }

    getFilterListElement :: Filter -> String -> Array String -> Boolean -> Widget HTML Filter
    getFilterListElement filter' label classes isSelected = li [Props.classList $ [if isSelected then Just "selected" else Nothing] <> (Just <$> classes), filter' <$ Props.onClick] [
      span [Props.className "label"] [ text label ]
    , span [Props.className "count"] [ int $ filterCardsNumber filter' ]
    ]

    filterCardsNumber :: Filter -> Int
    filterCardsNumber filter' =
      case filter' of
        Recent                -> min numberOfRecent (length $ shownEntries entries Nothing archived)
        filter_               -> length (filteredEntries filter_ $ shownEntries entries Nothing archived)

    min :: Int -> Int -> Int
    min n n' = if n < n' then n else n'

shownEntries :: List CardEntry -> Maybe CardEntry -> Boolean -> List CardEntry
shownEntries entries selectedEntry archived = List.filter (\(CardEntry r) -> archived || (not r.archived) || (Just (CardEntry r) == selectedEntry)) entries

filteredEntries :: Filter -> List CardEntry -> List CardEntry
filteredEntries filter = case filter of
  Search searchString'  -> if searchString' == ""
                           then identity
                           else List.filter (\(CardEntry entry) -> any (contains (Pattern (toLower searchString'))) (toLower <$> (entry.title : toUnfoldable entry.tags))) -- TODO: may be improved with a proper information retrieval system [fsolaroli - 27/11/2023]
  Tag    tag'           ->      List.filter (\(CardEntry entry) -> member  tag' entry.tags)                                                                     
  Untagged              ->      List.filter (\(CardEntry entry) -> isEmpty      entry.tags)                                                                     
  Recent                ->      List.sortBy (\(CardEntry e1) (CardEntry e2) -> compare e1.lastUsed e2.lastUsed) >>> List.takeEnd numberOfRecent
  All                   ->      identity      
