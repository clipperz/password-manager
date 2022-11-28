module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, ol, p')
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Semigroupoid ((<<<))
import Control.Bind (bind, discard)
import Data.Array (nub, sort)
import Data.EuclideanRing (mod)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra ((||), not)
import Data.List as List
import Data.List (fold, filter, length, List(..), (!!), elemIndex)
import Data.Maybe (Maybe(..))
import Data.PrettyShow (prettyShow)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((+))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import DataModel.AppState (AppError)
import DataModel.Card (Card)
import DataModel.Index (Index(..), CardEntry(..))
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import React.SyntheticEvent as Events
import Views.CardViews (cardView)
import Views.CreateCardView (createCardView)
import Views.IndexView (indexView, ComplexIndexFilter, IndexFilter(..), removeLastCardFilter, toFilterFunc, complexToFilterFunc)
import Views.SimpleWebComponents (simpleButton, loadingDiv, simpleCheckboxWidget, simpleTextInputWidgetWithFocus, clickableListItemWidget)
import OperationalWidgets.CardWidget (cardWidget)
import OperationalWidgets.CreateCardWidget (createCardWidget)

data CardViewAction = UpdateIndex IndexUpdateData | ShowCard CardEntry | ShowAddCard
instance showCardViewAction :: Show CardViewAction where
  show (UpdateIndex (IndexUpdateData a _)) = "UpdateIndex " <> show a
  show (ShowCard entry)  = "Show Card " <> show entry
  show  ShowAddCard    = "Show Add Card"

type CardViewState = { cardView :: CardView, cardViewState :: WidgetState }

data CardView = NoCard | CardFromReference CardEntry | JustCard Card | CardForm Card
instance showCardView :: Show CardView where
  show NoCard = "NoCard"
  show (CardFromReference cr) = "CardFromReference " <> show cr
  show (JustCard c) = "JustCard " <> show c
  show (CardForm c) = "CardForm " <> show c

data InternalAction = CardViewAction CardViewAction | ChangeFilter ComplexIndexFilter | KeyBoardAction Events.SyntheticKeyboardEvent 

type CardsViewInfo = {
  index :: Index
, indexFilter :: ComplexIndexFilter
, selectedIndexPosition :: Maybe Int
, cardViewState :: CardViewState
, error :: Maybe AppError
}

mkCardsViewInfo :: Index -> ComplexIndexFilter -> Maybe Int -> CardViewState -> Maybe AppError -> CardsViewInfo
mkCardsViewInfo index indexFilter selectedIndexPosition cardViewState error = { index, indexFilter, selectedIndexPosition, cardViewState, error }

-- cardsManagerView :: Boolean -> Index -> ComplexIndexFilter -> CardViewState -> Maybe AppError -> Widget HTML (Tuple ComplexIndexFilter CardViewAction)
cardsManagerView :: Boolean -> CardsViewInfo -> Widget HTML (Tuple CardsViewInfo CardViewAction)
cardsManagerView isOffline currentInfo@{ index: i@(Index entries)
                                       , indexFilter: cif@{archived, indexFilter}
                                       , selectedIndexPosition
                                       , cardViewState: cvs@{ cardView: cv, cardViewState } 
                                       , error} = do 
  let cEntry = case cv of 
                CardFromReference ce -> Just ce
                _ -> Nothing
  res <- div [Props._id "cardsManager", KeyBoardAction <$> Props.onKeyDown] $ (text <$> (fromMaybe $ prettyShow <$> error)) <> [
    ChangeFilter <$> div [Props._id "filterView"] [
      prepareFilter <$> ol [][
        getFilterListElement NoFilter "All"
      , getFilterListElement RecentFilter "Recent (TODO)"
      , getFilterListElement UntaggedFilter "Untagged"
      ]
    , (prepareFilter <<< GeneralFilter) <$> div [Props._id "generalFilterArea"] [simpleTextInputWidgetWithFocus "generalFilter" (text "Search") "Search" currentGeneralFilter]
    , prepareFilter <$> div [] [
      text "Tags"
      ,  ol [Props._id "tagFilter"] ((\tag -> getFilterListElement (TagFilter tag) tag) <$> shownSortedTags)
      ]
    , toggleArchivedButton
    ]
  , div [] [
      div [Props._id "filterHeader"] [
        text "tags",
        getFilterHeader indexFilter,
        text "menu"
      ]
    , div [Props._id "mainView" ] [
        div [Props._id "indexView"] [
          (CardViewAction <<< ShowCard) <$> indexView i cEntry cif -- TODO:
        , simpleButton "Add card" false (CardViewAction ShowAddCard) 
        ]
      , case cvs of
        { cardView: CardForm card,         cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate (Just card)) <$ createCardView card allSortedTags cardViewState
        { cardView: CardForm card,         cardViewState: _       } -> (CardViewAction  <<< UpdateIndex) <$> createCardWidget card allSortedTags cardViewState
        { cardView: CardFromReference ref, cardViewState: _       } -> (CardViewAction  <<< UpdateIndex) <$> cardWidget ref allSortedTags cardViewState
        { cardView: JustCard card,         cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate (Just card)) <$ (div [] [loadingDiv, cardView card isOffline])
        { cardView: JustCard card,         cardViewState: _       } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate (Just card)) <$ cardView card isOffline
        { cardView: NoCard       ,         cardViewState: _       } -> div [Props._id "card"] []
      ]
    ]
  ]
  case res of
    CardViewAction (ShowCard ref) -> cardsManagerView isOffline { index: i
                                                                , indexFilter: (removeLastCardFilter cif (Just ref))
                                                                , selectedIndexPosition: elemIndex ref sortedFilteredEntries
                                                                , cardViewState: { cardView: CardFromReference ref, cardViewState }
                                                                , error: Nothing }
    CardViewAction action -> pure $ Tuple (currentInfo { indexFilter = (removeLastCardFilter cif Nothing) }) action
    ChangeFilter newFilter -> do
      let f = complexToFilterFunc lastUses newFilter
      case cv of
        CardFromReference ref -> 
            cardsManagerView isOffline { index: i
                                       , indexFilter: newFilter
                                       , selectedIndexPosition
                                       , cardViewState: if f ref then cvs else { cardView: NoCard, cardViewState }
                                       , error: Nothing }
        _ -> cardsManagerView isOffline { index: i
                                        , indexFilter: newFilter
                                        , selectedIndexPosition
                                        , cardViewState: cvs
                                        , error: Nothing }
    KeyBoardAction ev -> do
      key <- liftEffect $ Events.key ev
      -- log $ "Key pressed: " <> key
      case key of
        "a" -> cardsManagerView isOffline closeCardInfo
        "ArrowLeft" -> cardsManagerView isOffline closeCardInfo
        "Escape" -> cardsManagerView isOffline closeCardInfo
        "d" -> cardsManagerView isOffline openCardInfo
        "ArrowRight" -> cardsManagerView isOffline openCardInfo
        "Enter" -> cardsManagerView isOffline openCardInfo
        "w" -> cardsManagerView isOffline moveUpInfo
        "ArrowUp" -> cardsManagerView isOffline moveUpInfo
        "s" -> cardsManagerView isOffline moveDownInfo
        "ArrowDown" -> cardsManagerView isOffline moveDownInfo
        _  -> cardsManagerView isOffline currentInfo

  where
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
      (\b -> { archived: b, indexFilter }) <$> div [Props._id "archivedFilterArea"] [ simpleCheckboxWidget 
                                                          "show_archived_checkbox" 
                                                          (div [] [ text "Show archived cards", div [] [text $ show countArchivedCards]]) 
                                                          true
                                                          archived
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
        ComposedAndFilter f' f'' -> p' [getFilterHeader f', text " and ", getFilterHeader f'']
        ComposedOrFilter f' f''  -> p' [getFilterHeader f', text " or ", getFilterHeader f'']
        GeneralFilter title      -> text title
        SpecificCardFilter _     -> text "last created card"
        TagFilter tag            -> text tag
        RecentFilter             -> text "recent"
        UntaggedFilter           -> text "untagged"
        NoFilter                 -> text "clipperz logo"

    lastUses = { allLastUses: (\(CardEntry r) -> r.lastUsed) <$> entries }

    countShownCards :: IndexFilter -> Int
    countShownCards RecentFilter = length $ filter (toFilterFunc lastUses RecentFilter) entries
    countShownCards indexFilt = length $ filter (toFilterFunc lastUses indexFilt) shownEntries

    getFilterListElement :: IndexFilter -> String -> Widget HTML IndexFilter
    getFilterListElement indexFilt s = clickableListItemWidget false (div [] [ text s, div [] [text $ show $ countShownCards indexFilt]]) [] indexFilt
