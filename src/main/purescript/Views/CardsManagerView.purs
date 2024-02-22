module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, div, header, li, ol, span, text)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Data.Array (fromFoldable, nub)
import Data.Eq ((/=), (==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.List (fold)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import DataModel.Card (Card, emptyCard)
import DataModel.Index (CardEntry(..), Index(..))
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.WidgetState (CardFormInput(..), CardManagerState, CardViewState(..))
import IndexFilterView (Filter(..), FilterData, FilterViewStatus(..), filteredEntries, getClassNameFromFilterStatus, indexFilterView, initialFilterData, shownEntries)
import Views.CardViews (CardEvent(..), cardView)
import Views.Components (proxyInfoComponent)
import Views.CreateCardView (createCardView)
import Views.SimpleWebComponents (simpleButton)

data CardManagerEvent = AddCardEvent                Card
                      | CloneCardEvent    CardEntry
                      | DeleteCardEvent   CardEntry
                      | EditCardEvent     CardEntry Card
                      | ArchiveCardEvent  CardEntry
                      | RestoreCardEvent  CardEntry
                      | OpenCardViewEvent CardEntry
                      | OpenUserAreaEvent

cardManagerInitialState :: CardManagerState
cardManagerInitialState = {
  filterData: initialFilterData
, selectedEntry: Nothing
, cardViewState: NoCard
}

data CardsManagerInternalEvent = StateUpdate CardManagerState | CardManagerEvent CardManagerEvent

cardsManagerView :: CardManagerState -> Index -> PasswordGeneratorSettings -> Widget HTML (Tuple CardManagerEvent CardManagerState)
cardsManagerView state@{filterData: filterData@{filterViewStatus, filter, archived}, selectedEntry, cardViewState} index@(Index list) userPasswordGeneratorSettings = do
  res <- div [Props._id "cardsManager", Props.className $ "filterView_" <> getClassNameFromFilterStatus filterViewStatus] [
    indexFilterView filterData index <#> updateFilterData
  , div [Props.className "cardToolbarFrame"] [
      toolbarHeader "frame"
    , proxyInfoComponent [Just "withDate"]
    , div [Props._id "mainView", Props.className (if cardViewState /= NoCard then "CardViewOpen" else "CardViewClose")] [
        div [Props._id "indexView"] [
          toolbarHeader "cardList"
        , div [Props.className "addCard"] [simpleButton "addCard" "add card" false (StateUpdate state { cardViewState = CardForm NewCard, selectedEntry = Nothing})]
        , indexView index selectedEntry filter archived <#> (CardManagerEvent <<< OpenCardViewEvent)
        ]
      , div [Props._id "card"] [
          mainStageView cardViewState
        ]
      ]
    ]
  ]

  case res of
    CardManagerEvent event    -> pure $ Tuple event state
    StateUpdate      newState -> cardsManagerView newState index userPasswordGeneratorSettings

  where
    updateFilterData :: FilterData -> CardsManagerInternalEvent --TODO: are these `update` functions useless with lenses? [fsolaroli - 28/11/2023]
    updateFilterData filterData' = StateUpdate (state { filterData = filterData' })

    updateCardView :: CardViewState -> CardManagerState
    updateCardView cardViewState' = (state { cardViewState = cardViewState' })

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
    , (CardManagerEvent OpenUserAreaEvent) <$ div [Props.className "menu"] [button [Props.onClick] [text "menu"]]
    ]

    allTags :: Array String
    allTags = nub $ fold $ (\(CardEntry entry) -> entry.tags) <$> fromFoldable list

    handleCardEvents :: CardEvent -> CardsManagerInternalEvent
    handleCardEvents (Edit    cardEntry card) = StateUpdate      $ updateCardView (CardForm $ ModifyCard card cardEntry)
    handleCardEvents (Clone   cardEntry     ) = CardManagerEvent (CloneCardEvent   cardEntry)
    handleCardEvents (Archive cardEntry     ) = CardManagerEvent (ArchiveCardEvent cardEntry)
    handleCardEvents (Restore cardEntry     ) = CardManagerEvent (RestoreCardEvent cardEntry)
    handleCardEvents (Delete  cardEntry     ) = CardManagerEvent (DeleteCardEvent  cardEntry)
    handleCardEvents (Exit                  ) = StateUpdate $ state { cardViewState = NoCard, selectedEntry = Nothing}

    mainStageView :: CardViewState -> Widget HTML CardsManagerInternalEvent
    mainStageView  NoCard                  = div [] []
    mainStageView (Card card cardEntry)    = cardView card cardEntry <#> handleCardEvents
    mainStageView (CardForm cardFormInput) = createCardView inputCard allTags userPasswordGeneratorSettings <#> (maybe (StateUpdate $ updateCardView viewCardStateUpdate) (CardManagerEvent <<< outputEvent))
      where

        inputCard = case cardFormInput of
          NewCard                    -> emptyCard
          NewCardFromFragment card   -> card
          ModifyCard          card _ -> card
        
        viewCardStateUpdate = case cardFormInput of
          NewCard                   -> NoCard
          NewCardFromFragment _     -> NoCard 
          ModifyCard card cardEntry -> Card card cardEntry

        outputEvent = case cardFormInput of
          NewCard                -> AddCardEvent
          NewCardFromFragment _  -> AddCardEvent
          ModifyCard _ cardEntry -> EditCardEvent cardEntry


-- ==================================================================                                                                                                                             

indexView :: Index -> Maybe CardEntry -> Filter -> Boolean -> Widget HTML CardEntry
indexView (Index entries) selectedEntry filter archived = ol [] (
  (fromFoldable sortedCards) <#> (\cardEntry@(CardEntry { title, archived: archived' }) -> 
    li [Props.classList [archivedClass archived', selectedClass cardEntry], cardEntry <$ Props.onClick] [
      text title
    ]
  )
) 

  where
    sortedCards = List.sort $ filteredEntries filter (shownEntries entries selectedEntry archived)
    archivedClass archived' = if archived'                   then Just "archived" else Nothing
    selectedClass entry     = if selectedEntry == Just entry then Just "selected" else Nothing