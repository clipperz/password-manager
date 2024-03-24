module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, dd, div, dl, dt, h3, h4, header, li, ol, span, text)
import Concur.React.Props as Props
import Control.Alt (($>), (<#>), (<|>))
import Control.Alternative ((*>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<))
import Data.Array (foldl, fromFoldable, mapWithIndex, nub)
import Data.CommutativeRing (add)
import Data.Either (Either(..))
import Data.Eq ((/=), (==))
import Data.Function (flip, (#), ($))
import Data.Functor ((<$>), (<$))
import Data.List (List, elemIndex, fold, index, length)
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.Ord (max, min)
import Data.Ring (sub, (-))
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.CardVersions.Card (Card, emptyCard)
import DataModel.IndexVersions.Index (CardEntry(..), Index(..))
import DataModel.Password (PasswordGeneratorSettings)
import DataModel.WidgetState (CardFormInput(..), CardManagerState, CardViewState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Events (blur, focus, keyboardShortcut)
import IndexFilterView (Filter(..), FilterData, FilterViewStatus(..), filteredEntries, getClassNameFromFilterStatus, indexFilterView, initialFilterData, shownEntries)
import Views.CardViews (CardEvent(..), cardView)
import Views.Components (proxyInfoComponent)
import Views.CreateCardView (createCardView)

data CardManagerEvent = AddCardEvent                Card
                      | CloneCardEvent    CardEntry
                      | DeleteCardEvent   CardEntry
                      | EditCardEvent     CardEntry Card
                      | ArchiveCardEvent  CardEntry
                      | RestoreCardEvent  CardEntry
                      | OpenCardViewEvent (Either (Maybe Int) CardEntry)
                      | OpenUserAreaEvent

cardManagerInitialState :: CardManagerState
cardManagerInitialState = {
  filterData: initialFilterData
, highlightedEntry: Nothing
, cardViewState: NoCard
}

data CardsManagerInternalEvent = StateUpdate CardManagerState | ShowShortcuts Boolean | CardManagerEvent CardManagerEvent

cardsManagerView :: CardManagerState -> Index -> PasswordGeneratorSettings -> Boolean -> Widget HTML (Tuple CardManagerEvent CardManagerState)
cardsManagerView state@{filterData: filterData@{filterViewStatus, filter, archived, searchString}, highlightedEntry, cardViewState} index'@(Index {entries}) userPasswordGeneratorSettings showShortcutsHelp = do
  let sortedCards = List.sort $ filteredEntries filter (shownEntries entries selectedEntry archived)
  res <- div [Props._id "cardsManager", Props.className $ "filterView_" <> getClassNameFromFilterStatus filterViewStatus] [
    indexFilterView filterData index' >>= updateFilterData
  , div [Props.className "cardToolbarFrame"] [
      toolbarHeader "frame"
    , proxyInfoComponent [Just "withDate"]
    , div [Props._id "mainView", Props.className (if cardViewState /= NoCard then "CardViewOpen" else "CardViewClose")] [
        div [Props._id "indexView"] [
          toolbarHeader "cardList"
        , div [Props.className "addCard"] [
            button [Props.onClick, Props.className "addCard" ] [span [] [text "add card"]] $> (StateUpdate state { cardViewState = CardForm NewCard })
          ]
        , (indexView sortedCards (getHighlightedEntry sortedCards)) <#> (CardManagerEvent <<< OpenCardViewEvent <<< Right)
        ]
      , div [Props._id "card"] [
          mainStageView cardViewState
        ]
      ]
    ]
  ] <> shortcutsHandlers sortedCards
    <> shortcutsHelp     showShortcutsHelp

  case res of
    CardManagerEvent event    -> pure $ Tuple event state
    StateUpdate      newState -> cardsManagerView newState index' userPasswordGeneratorSettings showShortcutsHelp
    ShowShortcuts    show     -> cardsManagerView state    index' userPasswordGeneratorSettings show

  where
    selectedEntry :: Maybe CardEntry
    selectedEntry = case cardViewState of
      Card                 _ entry  -> Just entry
      CardForm (ModifyCard _ entry) -> Just entry
      _                             -> Nothing

    getHighlightedEntry :: List CardEntry -> Maybe Int
    getHighlightedEntry entries' = highlightedEntry <|> (selectedEntry >>= flip elemIndex entries')

    increaseIndex :: List CardEntry -> Int -> Int
    increaseIndex entries' numberOfCards = min (numberOfCards-1) (maybe 0 (add 1)      (getHighlightedEntry entries'))
    
    decreaseIndex :: List CardEntry ->        Int
    decreaseIndex entries'               = max  0                (maybe 0 (flip sub 1) (getHighlightedEntry entries'))

    getCardToOpen :: List CardEntry -> Maybe CardEntry
    getCardToOpen entries' = highlightedEntry >>= (index entries')

    updateFilterData :: FilterData -> Widget HTML CardsManagerInternalEvent
    updateFilterData filterData' = 
      case filterData'.filterViewStatus of
        FilterViewClosed -> blur  "searchInputField" # liftEffect
        FilterViewOpen   -> 
          case filterData'.filter of
            Search _     -> focus "searchInputField" # liftEffect
            _            -> pure unit
      $> StateUpdate  ( state { filterData = filterData'
                              , highlightedEntry = Nothing 
                              }
                      )

    updateCardView :: CardViewState -> CardManagerState
    updateCardView cardViewState' = (state { cardViewState = cardViewState' })

    getFilterHeader :: forall a. Filter -> Widget HTML a
    getFilterHeader f =
      case f of
        All                  -> text "clipperz"
        Recent               -> text "recent"
        Untagged             -> text "untagged"
        Search ""            -> text "clipperz"
        Search searchString' -> span [] [text searchString']
        Tag    tag           -> span [] [text tag]

    toolbarHeader :: String -> Widget HTML CardsManagerInternalEvent
    toolbarHeader className = header [Props.className className] [
      div [Props.className "tags"] [button [Props.onClick] [text "tags"]] *> updateFilterData (filterData {filterViewStatus = FilterViewOpen})
    , div [Props.className "selection"] [getFilterHeader filter]
    , (CardManagerEvent OpenUserAreaEvent) <$ div [Props.className "menu"] [button [Props.onClick] [text "menu"]]
    ]

    allTags :: Array String
    allTags = nub $ fold $ (\(CardEntry entry) -> entry.tags) <$> fromFoldable entries

    shortcutsHandlers :: List CardEntry -> Widget HTML CardsManagerInternalEvent
    shortcutsHandlers sortedCards =
         ((keyboardShortcut ["*"                  ] # liftAff) *> updateFilterData initialFilterData)
      <> ((keyboardShortcut ["/"                  ] # liftAff) *> updateFilterData filterData {filterViewStatus = FilterViewOpen, filter = Search searchString})
      <> ((keyboardShortcut ["j", "down"          ] # liftAff) $> StateUpdate state {highlightedEntry = Just (increaseIndex sortedCards (length sortedCards))})
      <> ((keyboardShortcut ["k", "up"            ] # liftAff) $> StateUpdate state {highlightedEntry = Just (decreaseIndex sortedCards                     )})
      <> ((keyboardShortcut ["l", "right", "enter"] # liftAff) $> maybe (StateUpdate state) (CardManagerEvent <<< OpenCardViewEvent <<< Right) (getCardToOpen sortedCards))
      <> ((keyboardShortcut ["h", "left" , "esc"  ] # liftAff) $> if showShortcutsHelp
                                                                  then  (ShowShortcuts false)
                                                                  else  (CardManagerEvent <<< OpenCardViewEvent <<< Left ) (getHighlightedEntry sortedCards))
      <> ((keyboardShortcut ["?"                  ] # liftAff) $> ShowShortcuts true)

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

    handleCardEvents :: CardEvent -> CardsManagerInternalEvent
    handleCardEvents (Edit    cardEntry card) = StateUpdate      $ updateCardView (CardForm $ ModifyCard card cardEntry)
    handleCardEvents (Clone   cardEntry     ) = CardManagerEvent (CloneCardEvent   cardEntry)
    handleCardEvents (Archive cardEntry     ) = CardManagerEvent (ArchiveCardEvent cardEntry)
    handleCardEvents (Restore cardEntry     ) = CardManagerEvent (RestoreCardEvent cardEntry)
    handleCardEvents (Delete  cardEntry     ) = CardManagerEvent (DeleteCardEvent  cardEntry)
    handleCardEvents (Exit                  ) = StateUpdate $ state { cardViewState = NoCard }


-- ==================================================================                                                                                                                             

shortcutsHelp :: Boolean -> Widget HTML CardsManagerInternalEvent
shortcutsHelp showShortcutsHelp = div [Props.classList [Just "shortcutsHelp", Just "disableOverlay", hiddenClass]] [
  div [Props.className "mask", Props.onClick] []
, div [Props.className "helpBox"] [
      header [] [
        h3 [] [text "Keyboard shortcuts"]
      , button [Props.className "close", Props.onClick] [text "close"]
      ]
    , div [Props.className "helpContent"] [
        helpBlock "Search"      [ Tuple [ Tuple "/"        Nothing    ] "search cards"
                                , Tuple [ Tuple "*"        Nothing    ] "select all cards"
                                ]       
      , helpBlock "Navigation"  [ Tuple [ Tuple "h"       (Just "or")
                                        , Tuple "<left>"  (Just "or")
                                        , Tuple "<esc>"    Nothing
                                        ]                                "exit current selection"
                                , Tuple [ Tuple "l"       (Just "or")
                                        , Tuple "<right>" (Just "or")
                                        , Tuple "<enter>"  Nothing
                                        ]                                "select detail"   
                                , Tuple [ Tuple "k"        (Just "/")
                                        , Tuple "j"        (Just "or")
                                        , Tuple "<up>"     (Just "/")
                                        , Tuple "<down>"    Nothing
                                        ]                                "previous/next card"
                                ]
      , helpBlock "Misc"        [ Tuple [ Tuple "l o c k"   Nothing   ] "lock application"
                                ] 
    ]
  ]
] $> (ShowShortcuts false)
  where
    helpBlock :: forall a. String 
                        -> Array (Tuple (Array (Tuple String (Maybe String))) String) 
                        -> Widget HTML a
    helpBlock title shortcuts = 
      div [Props.className "helpBlock"] [
        h4 [] [text title]
      , dl [] $ foldl (\shortcutsList (Tuple shortcut description) -> shortcutsList <> [
          dt [] $ foldl (\shortcutCombination (Tuple key operator) -> 
               shortcutCombination <> [ span [Props.className "key"] [text key] ] <> case operator of
                                                                                      Just op -> [span [Props.className "operator"] [text op]]
                                                                                      Nothing -> []
          ) [] shortcut
        , dd [] [text description]
        ]) [] shortcuts
      ]
    
    hiddenClass :: Maybe String
    hiddenClass = if showShortcutsHelp
                  then Nothing
                  else Just "hidden"

-- ==================================================================                                                                                                                             

indexView :: List CardEntry -> Maybe Int -> Widget HTML CardEntry
indexView sortedCards selectedEntry = ol [] (
  flip mapWithIndex (fromFoldable sortedCards) (\index cardEntry@(CardEntry { title, archived: archived' }) -> 
    li [Props.classList [archivedClass archived', selectedClass index], cardEntry <$ Props.onClick] [
      text title
    ]
  )
) 

  where
    archivedClass archived' = if archived'                   then Just "archived" else Nothing
    selectedClass index     = if selectedEntry == Just index then Just "selected" else Nothing