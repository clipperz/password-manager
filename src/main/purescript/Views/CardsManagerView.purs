module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, ol, p')
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Semigroupoid ((<<<))
import Control.Bind (bind, discard)
import Data.Array (nub, sort)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra ((||), not)
import Data.List as List
import Data.List (fold, filter, length, List(..))
import Data.Maybe (Maybe(..))
import Data.PrettyShow (prettyShow)
import Data.Semigroup ((<>))
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
import Views.IndexView (indexView, ComplexIndexFilter, IndexFilter(..), toFilterFunc, complexToFilterFunc)
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

cardsManagerView :: Boolean -> Index -> ComplexIndexFilter -> CardViewState -> Maybe AppError -> Widget HTML (Tuple ComplexIndexFilter CardViewAction)
cardsManagerView isOffline i@(Index entries) cif@{archived, indexFilter} cvs@{ cardView: cv, cardViewState } error = do 
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
      div [Props._id "filterHeader"] [ getFilterHeader indexFilter ]
    , div [Props._id "mainView" ] [
        div [Props._id "indexView"] [
          (CardViewAction <<< ShowCard) <$> indexView i cEntry cif -- TODO:
        , simpleButton "Add card" false (CardViewAction ShowAddCard) 
        ]
      , case cvs of
        { cardView: CardForm card,         cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate card) <$ createCardView card allSortedTags cardViewState
        { cardView: CardForm card,         cardViewState: _       } -> (CardViewAction  <<< UpdateIndex) <$> createCardWidget card allSortedTags cardViewState
        { cardView: CardFromReference ref, cardViewState: _       } -> (CardViewAction  <<< UpdateIndex) <$> cardWidget ref allSortedTags cardViewState
        { cardView: JustCard card,         cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate card) <$ (div [] [loadingDiv, cardView card isOffline])
        { cardView: JustCard card,         cardViewState: _       } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate card) <$ cardView card isOffline
        { cardView: NoCard       ,         cardViewState: _       } -> div [Props._id "card"] []
      ]
    ]
  ]
  case res of
    CardViewAction (ShowCard ref) -> cardsManagerView isOffline i (removeLastCardFilter cif (Just ref)) { cardView: CardFromReference ref, cardViewState } Nothing -- TODO: discuss
    CardViewAction action -> pure $ Tuple (removeLastCardFilter cif Nothing) action
    ChangeFilter newFilter -> do
      let f = complexToFilterFunc lastUses newFilter
      case cv of
        CardFromReference ref -> if f ref then 
            cardsManagerView isOffline i newFilter cvs Nothing
          else 
            cardsManagerView isOffline i newFilter { cardView: NoCard, cardViewState } Nothing
        _ -> cardsManagerView isOffline i newFilter cvs Nothing
    KeyBoardAction ev -> do
      key <- liftEffect $ Events.key ev
      log $ "Key pressed: " <> key
      case key of
        "l" -> cardsManagerView isOffline i cif { cardView: NoCard, cardViewState: Default } Nothing -- using a variable to factorize this behaviour breaks everything
        "ArrowLeft" -> cardsManagerView isOffline i cif { cardView: NoCard, cardViewState: Default } Nothing
        "Escape" -> cardsManagerView isOffline i cif { cardView: NoCard, cardViewState: Default } Nothing
        "h" -> 
          case sortedFilteredEntries of
            Nil -> cardsManagerView isOffline i cif { cardView: NoCard, cardViewState: Default } Nothing
            Cons ref _ -> cardsManagerView isOffline i cif { cardView: CardFromReference ref, cardViewState: Default } Nothing
        "ArrowRight" -> 
          case sortedFilteredEntries of
            Nil -> cardsManagerView isOffline i cif { cardView: NoCard, cardViewState: Default } Nothing
            Cons ref _ -> cardsManagerView isOffline i cif { cardView: CardFromReference ref, cardViewState: Default } Nothing
        "Enter" -> 
          case sortedFilteredEntries of
            Nil -> cardsManagerView isOffline i cif { cardView: NoCard, cardViewState: Default } Nothing
            Cons ref _ -> cardsManagerView isOffline i cif { cardView: CardFromReference ref, cardViewState: Default } Nothing
        _  -> cardsManagerView isOffline i cif cvs Nothing

  where
    removeLastCardFilter cf@{ archived: archived', indexFilter: indexFilter' } mRef =
      case indexFilter' of
        ComposedAndFilter (SpecificCardFilter ce) filter -> if (Just ce) == mRef then cf else{ archived: archived', indexFilter: filter }
        ComposedAndFilter filter (SpecificCardFilter ce) -> if (Just ce) == mRef then cf else{ archived: archived', indexFilter: filter }
        ComposedOrFilter (SpecificCardFilter ce) filter -> if (Just ce) == mRef then cf else{ archived: archived', indexFilter: filter }
        ComposedOrFilter filter (SpecificCardFilter ce) -> if (Just ce) == mRef then cf else{ archived: archived', indexFilter: filter }
        SpecificCardFilter ce -> if (Just ce) == mRef then cf else { archived: archived', indexFilter: NoFilter }
        _ -> cf

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
