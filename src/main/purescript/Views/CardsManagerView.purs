module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, header, input, label, li, number, ol, span, text)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category (identity, (<<<), (>>>))
import Data.Array (any, elem, fromFoldable, nub, null, sort, (:))
import Data.Eq (class Eq, (==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra ((||), (&&), not)
import Data.Int (toNumber)
import Data.List (List, fold, length)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Ord (compare, (<))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String (Pattern(..), contains, toLower)
import DataModel.AppState (ProxyConnectionStatus(..))
import DataModel.Card (Card, emptyCard)
import DataModel.Index (CardEntry(..), Index(..))
import Effect.Console (debug)
import OperationalWidgets.CreateCardWidget (CardFormInput(..))
import Views.CardViews (cardView)
import Views.SimpleWebComponents (simpleButton, simpleTextInputWidgetWithFocus)


data CardManagerEvent = AddCardEvent Card
                      | DeleteCardEvent -- ??
                      | EditCardEvent -- ??
                      | OpenCardViewEvent CardEntry
                      | OpenUserAreaEvent CardsManagerState

data CardView = NoCard | CardFromReference CardEntry | JustCard Card | CardForm CardFormInput

type CardsManagerState = { 
  filterData    :: FilterData
, selectedEntry :: Maybe CardEntry
, cardView      :: CardView
}

cardsManagerInitialState :: CardsManagerState
cardsManagerInitialState = {
  filterData: initialFilterData
, selectedEntry: Nothing
, cardView: NoCard
}

data CardsManagerInternalEvent = CardManagerEvent CardManagerEvent | StateUpdate CardsManagerState

getClassNameFromFilterStatus :: FilterViewStatus -> String
getClassNameFromFilterStatus status = case status of
  FilterViewClosed  -> "closed"
  FilterViewOpen    -> "open"


cardsManagerView :: CardsManagerState -> Index -> Widget HTML CardManagerEvent
cardsManagerView state@{filterData: filterData@{filterViewStatus, filter}, selectedEntry} index = do
  res <- div [Props._id "cardsManager", Props.className $ "filterView_" <> getClassNameFromFilterStatus filterViewStatus] [
    indexFilterView filterData index <#> updateFilterData
  , div [Props.className "cardToolbarFrame"] [
      toolbarHeader "frame"
    , div [Props._id "mainView"{-, Props.className $ getMainViewClassFromCardState cvs-}] [
        div [Props._id "indexView"] [
          toolbarHeader "cardList"
        , div [Props.className "addCard"] [simpleButton "addCard" "add card" false (updateCardView (CardForm $ NewCard Nothing))]
        , indexView index selectedEntry filter <#> (CardManagerEvent <<< OpenCardViewEvent)
        ]
      , div [Props._id "card"] [
          StateUpdate state <$ cardView emptyCard ProxyOnline
        ]
      ]
    ]
  ]

  case res of
    CardManagerEvent event    -> pure event
    StateUpdate      newState -> cardsManagerView newState index

  where
    updateFilterData :: FilterData -> CardsManagerInternalEvent --TODO: are these `update` functions useless with lents? [fsolaroli - 28/11/2023]
    updateFilterData filterData' = StateUpdate (state { filterData = filterData' })

    updateCardView :: CardView -> CardsManagerInternalEvent
    updateCardView cardView' = StateUpdate (state { cardView = cardView' })

    getFilterHeader :: forall a. Filter -> Widget HTML a
    getFilterHeader f =
      case f of
        Search searchString -> span [] [text searchString]
        Tag    tag          -> span [] [text tag]
        Recent              -> text "recent"
        Untagged            -> text "untagged"
        All                 -> text "clipperz"

    toolbarHeader :: String -> Widget HTML CardsManagerInternalEvent
    toolbarHeader className = header [Props.className className] [
      div [Props.className "tags"] [button [Props.onClick] [text "tags"]] $> updateFilterData (filterData {filterViewStatus = FilterViewOpen})
    , div [Props.className "selection"] [getFilterHeader filter]
    , (CardManagerEvent $ OpenUserAreaEvent state) <$ div [Props.className "menu"] [button [Props.onClick] [text "menu"]]
    ]

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

initialFilterData :: FilterData
initialFilterData = {
  archived: false
, filter:   All
, filterViewStatus: FilterViewClosed
, searchString: ""
}

indexFilterView :: FilterData -> Index -> Widget HTML FilterData
indexFilterView filterData@{archived, filter, searchString} (Index entries) = div [Props._id "filterView"] [
    (filterData {filterViewStatus = FilterViewClosed}) <$ div [debug "CLOSE" <$ Props.onClick, Props.className "mask"] []
  , div [Props.className "content"] [
      div [Props.className "filter"] [
        ol [Props.className "defaultSets"] [
          getFilterListElement All      "All"           ["allCards"]      (filter == All && searchString == "")
        , getFilterListElement Recent   "Recent (TODO)" ["recentCards"]   (filter == Recent)
        , getFilterListElement Untagged "Untagged"      ["untaggedCards"] (filter == Untagged)
        ] <#> updateFilter
      , div [Props._id "searchForm", Props.classList [searchFormClassName]] [
          simpleTextInputWidgetWithFocus "search" (text "search") "search" searchString
        , span [Props.className "count"] [number $ filterCardsNumber (Search searchString)]
        ] <#> (\search -> filterData {filter = Search search, searchString = search})
      , div [Props.className "tags"] [
          span [Props.className "tags"] [text "Tags"]
        , ol [Props._id "tagFilter"] ((\tag -> getFilterListElement (Tag tag) tag [] (filter == Tag tag)) <$> (sort $ nub $ fold $ (\(CardEntry { tags }) -> tags) <$> shownEntries))
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
      , span [Props.className "count"] [text archivedCardsNumber]
      ]
    ]
  ]

  where
    searchFormClassName = case filter of
      Search "" -> Nothing
      Search _  -> Just "selected"
      _         -> Nothing

    archivedCardsNumber = show $ length $ List.filter (\(CardEntry r) -> r.archived) entries

    shownEntries :: List CardEntry
    shownEntries = List.filter (\(CardEntry r) -> archived || (not r.archived)) entries

    updateFilter :: Filter -> FilterData
    updateFilter newFilter = filterData { filter = newFilter }

    getFilterListElement :: Filter -> String -> Array String -> Boolean -> Widget HTML Filter
    getFilterListElement filter' label classes isSelected = li [Props.classList $ [if isSelected then Just "selected" else Nothing] <> (Just <$> classes), filter' <$ Props.onClick] [
      span [Props.className "label"] [ text label ]
    , span [Props.className "count"] [ number $ filterCardsNumber filter' ]
    ]

    filterCardsNumber :: Filter -> Number
    filterCardsNumber filter' = toNumber $
      case filter' of
        Recent                -> min numberOfRecent (length shownEntries)
        filter_               -> length (filteredEntries filter_ shownEntries)

    min :: Int -> Int -> Int
    min n n' = if n < n' then n else n'

-- ==================================================================

filteredEntries :: Filter -> List CardEntry -> List CardEntry
filteredEntries filter = case filter of
  Search searchString'  -> if searchString' == ""
                                  then identity
                                  else List.filter (\(CardEntry entry) -> any (contains (Pattern (toLower searchString'))) (toLower <$> (entry.title : entry.tags))) -- TODO: may be improved with a proper information retrieval system [fsolaroli - 27/11/2023]
  Tag    tag'           ->             List.filter (\(CardEntry entry) -> elem tag' entry.tags)                                                                     
  Untagged              ->             List.filter (\(CardEntry entry) -> null      entry.tags)                                                                     
  Recent                ->             List.sortBy (\(CardEntry e1) (CardEntry e2) -> compare e1.lastUsed e2.lastUsed) >>> List.takeEnd numberOfRecent
  All                   ->             identity                                                                                                                                   

indexView :: Index -> Maybe CardEntry -> Filter -> Widget HTML CardEntry
indexView (Index entries) selectedEntry filter = ol [] (
  (fromFoldable sortedCards) <#> (\cardEntry@(CardEntry { title, archived }) -> 
    li [Props.classList [archivedClass archived, selectedClass cardEntry], cardEntry <$ Props.onClick] [
      text title
    ]
  )
) 

  where
    sortedCards = List.sort $ filteredEntries filter entries
    archivedClass archived = if archived                    then Just "archived" else Nothing
    selectedClass entry    = if selectedEntry == Just entry then Just "selected" else Nothing


-- ================================================================== --TODO: implement [fsolaroli - 28/11/2023]

-- mainStageView

-- createCardView emptyCard [] false WidgetState.Default <#> (maybe (updateCardView NoCard) (CardManagerEvent <<< AddCardEvent))

-- ==================================================================
