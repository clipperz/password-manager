module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, ol, p', button, header, span)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Semigroupoid ((<<<))
import Data.Array (nub, sort)
import Data.Eq ((==), (/=))
import Data.EuclideanRing (mod)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra ((||), (&&), not)
import Data.List (fold, filter, length, List(..), (!!), elemIndex)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.PrettyShow (prettyShow)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import DataModel.AppState (AppError, ProxyConnectionStatus)
import DataModel.Card (Card, emptyCard)
import DataModel.Index (Index(..), CardEntry(..))
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Class (liftEffect)
import OperationalWidgets.CardWidget (cardWidget)
import OperationalWidgets.CreateCardWidget (CardFormInput(..), createCardWidget)
import React.SyntheticEvent as Events
import Views.CardViews (cardView)
import Views.CreateCardView (createCardView)
import Views.IndexView (indexView, ComplexIndexFilter, IndexFilter(..), removeLastCardFilter, toFilterFunc, complexToFilterFunc)
import Views.SimpleWebComponents (simpleButton, loadingDiv, simpleCheckboxWidget, simpleTextInputWidgetWithFocus, clickableListItemWidget)

data CardViewAction = UpdateIndex IndexUpdateData | ShowCard CardEntry | ShowAddCard | ShowUserArea
instance showCardViewAction :: Show CardViewAction where
  show (UpdateIndex (IndexUpdateData a _)) = "UpdateIndex " <> show a
  show (ShowCard entry)  = "Show Card " <> show entry
  show  ShowAddCard    = "Show Add Card"
  show  ShowUserArea   = "Show User Area"

type CardViewState = { cardView :: CardView, cardViewState :: WidgetState }

data CardView = NoCard | CardFromReference CardEntry | JustCard Card | CardForm CardFormInput
instance showCardView :: Show CardView where
  show NoCard = "NoCard"
  show (CardFromReference cr) = "CardFromReference " <> show cr
  show (JustCard c) = "JustCard " <> show c
  show (CardForm (NewCard c)) = "CardForm - NewCard " <> show c
  show (CardForm (ModifyCard c)) = "CardForm - ModifyCard " <> show c

data InternalAction = CardViewAction CardViewAction | ChangeFilter ComplexIndexFilter | KeyBoardAction Events.SyntheticKeyboardEvent | ShowFilters FilterViewStatus

type CardsViewInfo = {
  index :: Index
, indexFilter :: ComplexIndexFilter
, selectedIndexPosition :: Maybe Int
, cardViewState :: CardViewState
, error :: Maybe AppError
}

data FilterViewStatus = FilterViewClosed | FilterViewOpen

mkCardsViewInfo :: Index -> ComplexIndexFilter -> Maybe Int -> CardViewState -> Maybe AppError -> CardsViewInfo
mkCardsViewInfo index indexFilter selectedIndexPosition cardViewState error = { index, indexFilter, selectedIndexPosition, cardViewState, error }

getClassNameFromFilterStatus :: FilterViewStatus -> String
getClassNameFromFilterStatus status = case status of
  FilterViewClosed  -> "closed"
  FilterViewOpen    -> "open"

cardsManagerView :: ProxyConnectionStatus -> FilterViewStatus -> CardsViewInfo -> Widget HTML (Tuple CardsViewInfo CardViewAction)
cardsManagerView proxyConnectionStatus filterViewStatus currentInfo@{ index: i@(Index entries)
                                                                    , indexFilter: cif@{archived, indexFilter}
                                                                    , selectedIndexPosition
                                                                    , cardViewState: cvs@{ cardView: cv, cardViewState } 
                                                                    , error} = do 
  let cEntry = case cv of 
                CardFromReference ce -> Just ce
                _ -> Nothing
  res <- div [Props._id "cardsManager", KeyBoardAction <$> Props.onKeyDown, Props.className $ "filterView_" <> getClassNameFromFilterStatus filterViewStatus] $ (text <$> (fromMaybe $ prettyShow <$> error)) <> [
    div [Props._id "filterView"] [
      (ShowFilters FilterViewClosed) <$ div [Props.onClick, Props.className "mask"] []
    , ChangeFilter <$> div [Props.className "content"] [
        prepareFilter <$> div [Props.className "filter"] [
          ol [Props.className "defaultSets"][
            getFilterListElement NoFilter "All" ["allCards"]
          , getFilterListElement RecentFilter "Recent (TODO)" ["recentCards"]
          , getFilterListElement UntaggedFilter "Untagged" ["untaggedCards"]
          ]
        , GeneralFilter <$> div [Props._id "searchForm", Props.className (if (indexFilter == GeneralFilter currentGeneralFilter && currentGeneralFilter /= "") then "selected" else  "")] [
            simpleTextInputWidgetWithFocus "search" (text "search") "search" currentGeneralFilter
          , span [Props.className "count"] [text $ show $ countShownCards (GeneralFilter currentGeneralFilter)]
          ]
        , div [Props.className "tags"] [
            span [Props.className "tags"] [text "Tags"]
          , ol [Props._id "tagFilter"] ((\tag -> getFilterListElement (TagFilter tag) tag []) <$> shownSortedTags)
          ]
        ]
      , toggleArchivedButton
      ]
    ]
  , div [Props.className "cardToolbarFrame"] [
    --   div [Props._id "filterHeader"] [
      toolbarHeader "frame"
    , div [Props._id "mainView", Props.className $ getMainViewClassFromCardState cvs] [
        div [Props._id "indexView"] [
          toolbarHeader "cardList"
        , div [Props.className "addCard"] [simpleButton "addCard" "add card" false (CardViewAction ShowAddCard)]
        , (CardViewAction <<< ShowCard) <$> indexView i cEntry cif -- TODO:
        ]
      , div [Props._id "card"] [
          case cvs of
          { cardView: CardForm (ModifyCard card),     cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate (Just card)) <$ createCardView card      allSortedTags false cardViewState
          { cardView: CardForm (NewCard (Just card)), cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate (Just card)) <$ createCardView card      allSortedTags false cardViewState
          { cardView: CardForm (NewCard Nothing),     cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate Nothing)     <$ createCardView emptyCard allSortedTags false cardViewState
          { cardView: CardForm card,                  cardViewState: _       } ->  (CardViewAction <<< UpdateIndex) <$> createCardWidget card allSortedTags cardViewState
          { cardView: CardFromReference ref,          cardViewState: _       } ->  (CardViewAction <<< UpdateIndex) <$> cardWidget ref allSortedTags cardViewState
          { cardView: JustCard card,                  cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate (Just card)) <$ (div [] [loadingDiv, cardView card proxyConnectionStatus])
          { cardView: JustCard card,                  cardViewState: _       } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate (Just card)) <$ cardView card proxyConnectionStatus
          { cardView: NoCard       ,                  cardViewState: _       } -> div [] []
        ]
      ]
    ]
  ]
  case res of
    CardViewAction (ShowCard ref) -> cardsManagerView proxyConnectionStatus filterViewStatus {
        index: i
      , indexFilter: (removeLastCardFilter cif (Just ref))
      , selectedIndexPosition: elemIndex ref sortedFilteredEntries
      , cardViewState: { cardView: CardFromReference ref, cardViewState }
      , error: Nothing
    }
    CardViewAction action -> pure $ Tuple (currentInfo { indexFilter = (removeLastCardFilter cif Nothing) }) action
    ShowFilters status -> cardsManagerView proxyConnectionStatus status currentInfo
    ChangeFilter newFilter -> do
      let f = complexToFilterFunc lastUses newFilter
      case cv of
        CardFromReference ref ->  cardsManagerView proxyConnectionStatus (setFilterViewStatus indexFilter) {
            index: i
          , indexFilter: newFilter
          , selectedIndexPosition
          , cardViewState: if f ref then cvs else { cardView: NoCard, cardViewState }
          , error: Nothing
        }
        _ -> cardsManagerView proxyConnectionStatus (setFilterViewStatus indexFilter) {
            index: i
          , indexFilter: newFilter
          , selectedIndexPosition
          , cardViewState: cvs
          , error: Nothing
        }
    KeyBoardAction ev -> do
      key <- liftEffect $ Events.key ev
      case key of
        "a" ->          keyboardAction closeCardInfo
        "ArrowLeft" ->  keyboardAction closeCardInfo
        "Escape" ->     keyboardAction closeCardInfo
        "d" ->          keyboardAction openCardInfo
        "ArrowRight" -> keyboardAction openCardInfo
        "Enter" ->      keyboardAction openCardInfo
        "w" ->          keyboardAction moveUpInfo
        "ArrowUp" ->    keyboardAction moveUpInfo
        "s" ->          keyboardAction moveDownInfo
        "ArrowDown" ->  keyboardAction moveDownInfo
        _  ->           keyboardAction currentInfo
      where keyboardAction = cardsManagerView proxyConnectionStatus filterViewStatus

  where
    getMainViewClassFromCardState cvs_ =
      case cvs_ of
        { cardView: NoCard,               cardViewState: _ }        -> "NoCard"
        { cardView: _,                    cardViewState: Loading }  -> "Loading"
        { cardView: CardForm _,           cardViewState: _       }  -> "CardForm"
        { cardView: CardFromReference _,  cardViewState: _ }        -> "CardFromReference"
        { cardView: JustCard _,           cardViewState: _ }        -> "JustCard"

    toolbarHeader className = 
      header [Props.className className] [
        (ShowFilters FilterViewOpen) <$ div [Props.className "tags"] [button [Props.onClick] [text "tags"]],
        div [Props.className "selection"] [getFilterHeader indexFilter],
        (CardViewAction ShowUserArea) <$ div [Props.className "menu"] [button [Props.onClick] [text "menu"]]
      ]

    setFilterViewStatus filter = case filter of
      GeneralFilter _ -> FilterViewOpen
      _ -> FilterViewClosed

    closeCardInfo = 
      case cv of
        CardForm _ -> currentInfo
        _ -> { index: i
             , indexFilter: cif
             , selectedIndexPosition
             , cardViewState: { cardView: NoCard, cardViewState: Default }
             , error: Nothing }
    openCardInfo = 
      case cv of
        CardForm _ -> currentInfo
        _ -> case selectedIndexPosition of
          Nothing -> case sortedFilteredEntries of
              Nil -> currentInfo
              Cons ref _ -> currentInfo { selectedIndexPosition = Just 0, cardViewState = { cardView: CardFromReference ref, cardViewState: Default } }
          Just n -> case sortedFilteredEntries !! n of
            Nothing -> currentInfo
            Just ref -> currentInfo { cardViewState = { cardView: CardFromReference ref, cardViewState: Default } }
    moveUpInfo =
      case cv of
        NoCard -> currentInfo --{ selectedIndexPosition = ((\n -> (n - 1) `mod` (length shownEntries)) <$> selectedIndexPosition) }
        CardForm _ -> currentInfo
        _ -> case selectedIndexPosition of
          Nothing -> currentInfo { selectedIndexPosition = Just 0 }
          Just n -> let newN = (n - 1) `mod` (length shownEntries)
                    in case sortedFilteredEntries !! newN of
                      Nothing -> currentInfo
                      Just ref -> currentInfo { selectedIndexPosition = Just newN, cardViewState = {cardView: CardFromReference ref, cardViewState: Default}}          
    moveDownInfo =
      case cv of
        NoCard -> currentInfo --{ selectedIndexPosition = ((\n -> (n + 1) `mod` (length shownEntries)) <$> selectedIndexPosition) }
        CardForm _ -> currentInfo
        _ -> case selectedIndexPosition of
          Nothing -> currentInfo { selectedIndexPosition = Just 0 }
          Just n -> let newN = (n + 1) `mod` (length shownEntries)
                    in case sortedFilteredEntries !! newN of
                      Nothing -> currentInfo
                      Just ref -> currentInfo { selectedIndexPosition = Just newN, cardViewState = {cardView: CardFromReference ref, cardViewState: Default}}

    toggleArchivedButton = 
      (\b -> { archived: b, indexFilter }) <$> div [Props._id "archivedFilterArea"] [
        simpleCheckboxWidget "showArchived" (text "Show archived cards") archived
      , (span [Props.className "count"] [text $ show countArchivedCards])
      ]
    countArchivedCards = length $ filter (\(CardEntry r) -> r.archived) entries

    prepareFilter :: IndexFilter -> ComplexIndexFilter
    prepareFilter newFilter = {archived, indexFilter: newFilter}

    currentGeneralFilter = case indexFilter of
      GeneralFilter t -> t
      _ -> ""

    sortedFilteredEntries = List.sort $ filter (toFilterFunc lastUses indexFilter) shownEntries

    allSortedTags = sort $ nub $ fold $ (\(CardEntry { tags }) -> tags) <$> entries

    shownEntries = filter (\(CardEntry r) -> archived || (not r.archived)) entries

    shownSortedTags = sort $ nub $ fold $ (\(CardEntry { tags }) -> tags) <$> shownEntries

    getFilterHeader :: forall a. IndexFilter -> Widget HTML a
    getFilterHeader f =
      case f of
        ComposedAndFilter f' f'' -> p' [getFilterHeader f', getFilterHeader f'']
        ComposedOrFilter f' f''  -> p' [getFilterHeader f', getFilterHeader f'']
        GeneralFilter title      -> span [] [text title]
        TagFilter tag            -> span [] [text tag]
        RecentFilter             -> text "recent"
        UntaggedFilter           -> text "untagged"
        NoFilter                 -> text "clipperz"
        _                        -> text ""

    lastUses = { allLastUses: (\(CardEntry r) -> r.lastUsed) <$> entries }

    countShownCards :: IndexFilter -> Int
    countShownCards RecentFilter = length $ filter (toFilterFunc lastUses RecentFilter) entries
    countShownCards indexFilt = length $ filter (toFilterFunc lastUses indexFilt) shownEntries

    getFilterListElement :: IndexFilter -> String -> Array String -> Widget HTML IndexFilter
    -- getFilterListElement indexFilt s classes = clickableListItemWidget false (div [] [ text s, div [] [text $ show $ countShownCards indexFilt]]) classes indexFilt
    getFilterListElement indexFilt s classes = clickableListItemWidget false (span [Props.className "label"] [ text s ] <|> span [Props.className "count"] [text $ show $ countShownCards indexFilt]) (if indexFilter == indexFilt then (classes <> ["selected"]) else classes) indexFilt
