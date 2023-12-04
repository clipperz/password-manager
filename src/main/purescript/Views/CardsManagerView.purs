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
import Data.Eq (class Eq, (/=), (==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.List (fold)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Semigroup ((<>))
import DataModel.Card (Card, emptyCard)
import DataModel.Index (CardEntry(..), Index(..))
import DataModel.Password (PasswordGeneratorSettings)
import IndexFilterView (Filter(..), FilterData, FilterViewStatus(..), filteredEntries, getClassNameFromFilterStatus, indexFilterView, initialFilterData)
import Views.CardViews (CardEvent(..), cardView)
import Views.CreateCardView (createCardView)
import Views.SimpleWebComponents (simpleButton)

data CardFormInput = NewCard (Maybe Card) | ModifyCard Card CardEntry -- TODO NewCard | NewCardFromFragment Card | ModifyCard Card CardEntry [fsolaroli - 03/12/2023]
derive instance eqCardFormInput :: Eq CardFormInput

data CardManagerEvent = AddCardEvent                Card
                      | CloneCardEvent    CardEntry
                      | DeleteCardEvent   CardEntry
                      | EditCardEvent     CardEntry Card
                      | ArchiveCardEvent  CardEntry
                      | RestoreCardEvent  CardEntry
                      | OpenCardViewEvent CardsManagerState CardEntry
                      | OpenUserAreaEvent CardsManagerState

data CardViewState = NoCard | Card Card CardEntry | CardForm CardFormInput
derive instance eqCardViewState :: Eq CardViewState

type CardsManagerState = { 
  filterData    :: FilterData
, selectedEntry :: Maybe CardEntry
, cardViewState :: CardViewState
}

cardsManagerInitialState :: CardsManagerState
cardsManagerInitialState = {
  filterData: initialFilterData
, selectedEntry: Nothing
, cardViewState: NoCard
}

data CardsManagerInternalEvent = StateUpdate CardsManagerState | CardManagerEvent CardManagerEvent

cardsManagerView :: CardsManagerState -> Index -> PasswordGeneratorSettings -> Widget HTML CardManagerEvent
cardsManagerView state@{filterData: filterData@{filterViewStatus, filter}, selectedEntry, cardViewState} index@(Index list) userPasswordGeneratorSettings = do
  res <- div [Props._id "cardsManager", Props.className $ "filterView_" <> getClassNameFromFilterStatus filterViewStatus] [
    indexFilterView filterData index <#> updateFilterData
  , div [Props.className "cardToolbarFrame"] [
      toolbarHeader "frame"
    , div [Props._id "mainView", Props.className (if cardViewState /= NoCard then "CardViewOpen" else "CardViewClose")] [
        div [Props._id "indexView"] [
          toolbarHeader "cardList"
        , div [Props.className "addCard"] [simpleButton "addCard" "add card" false (StateUpdate state { cardViewState = CardForm $ NewCard Nothing, selectedEntry = Nothing})]
        , indexView index selectedEntry filter <#> (CardManagerEvent <<< OpenCardViewEvent state)
        ]
      , div [Props._id "card"] [
          mainStageView cardViewState
        ]
      ]
    ]
  ]

  case res of
    CardManagerEvent event    -> pure event
    StateUpdate      newState -> cardsManagerView newState index userPasswordGeneratorSettings

  where
    updateFilterData :: FilterData -> CardsManagerInternalEvent --TODO: are these `update` functions useless with lenses? [fsolaroli - 28/11/2023]
    updateFilterData filterData' = StateUpdate (state { filterData = filterData' })

    updateCardView :: CardViewState -> CardsManagerState
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
    , (CardManagerEvent $ OpenUserAreaEvent state) <$ div [Props.className "menu"] [button [Props.onClick] [text "menu"]]
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
    mainStageView NoCard                   = div [] []
    mainStageView (Card card cardEntry)    = cardView card cardEntry <#> handleCardEvents
    mainStageView (CardForm cardFormInput) = createCardView inputCard allTags userPasswordGeneratorSettings <#> (maybe (StateUpdate $ updateCardView viewCardStateUpdate) (CardManagerEvent <<< outputEvent))
      where

        inputCard = case cardFormInput of
          NewCard   (Just card) -> card
          NewCard    Nothing    -> emptyCard
          ModifyCard card _     -> card
        
        viewCardStateUpdate = case cardFormInput of
          NewCard    _    -> NoCard
          ModifyCard card cardEntry -> Card card cardEntry

        outputEvent = case cardFormInput of
          NewCard    _           -> AddCardEvent
          ModifyCard _ cardEntry -> EditCardEvent cardEntry


-- ==================================================================                                                                                                                             

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