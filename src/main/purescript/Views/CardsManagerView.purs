module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, ol, p')
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Semigroupoid ((<<<))
import Control.Bind (bind, discard)
import Data.Array (nub, sort, null)
import Data.Array.NonEmpty as NE
import Data.Array.NonEmpty (toArray, singleton, fromArray, (:))
import Data.Eq ((==), (/=))
import Data.Foldable (any)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HeytingAlgebra ((||), not)
import Data.List (fold, filter, length, toUnfoldable, List(..))
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
import Effect.Class.Console (log)
import Views.CardViews (cardView)
import Views.CreateCardView (createCardView)
import Views.IndexView (indexView, ComplexIndexFilter, IndexFilter(..), toFilterFunc)
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

data InternalAction = CardViewAction CardViewAction | ChangeFilter ComplexIndexFilter

cardsManagerView :: Index -> ComplexIndexFilter -> CardViewState -> Maybe AppError -> Widget HTML (Tuple ComplexIndexFilter CardViewAction)
cardsManagerView i@(Index entries) cif@{archived, indexFilter} cvs@{ cardView: _, cardViewState } error = do 
  res <- div [Props._id "cardsManager"] $ (text <$> (fromMaybe $ prettyShow <$> error)) <> [
    ChangeFilter <$> div [Props._id "filterView"] [
      prepareFilter <$> ol [][
        getFilterListElement NoFilter "All"
      , getFilterListElement RecentFilter "Recent (TODO)"
      , getFilterListElement UntaggedFilter "Untagged"
      ]
    , (prepareFilter <<< TitleFilter) <$> div [Props._id "generalFilterArea"] [simpleTextInputWidgetWithFocus "titleFilter" (text "Title") "Card title" currentTitleFilter]
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
          (CardViewAction <<< ShowCard) <$> indexView i cif
        , simpleButton "Add card" false (CardViewAction ShowAddCard) 
        ]
      , case cvs of
        { cardView: CardForm card,         cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate card) <$ createCardView card allSortedTags cardViewState
        { cardView: CardForm card,         cardViewState: _       } -> (CardViewAction  <<< UpdateIndex) <$> createCardWidget card allSortedTags cardViewState
        { cardView: CardFromReference ref, cardViewState: _       } -> (CardViewAction  <<< UpdateIndex) <$> cardWidget ref allSortedTags cardViewState
        { cardView: JustCard card,         cardViewState: Loading } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate card) <$ (div [] [loadingDiv, cardView card])
        { cardView: JustCard card,         cardViewState: _       } -> ((CardViewAction <<< UpdateIndex)  $  IndexUpdateData NoUpdate card) <$ cardView card
        { cardView: NoCard       ,         cardViewState: _       } -> div [Props._id "card"] []
      ]
    ]
  ]
  case res of
    CardViewAction (ShowCard ref) -> cardsManagerView i (removeLastCardFilter cif (Just ref)) { cardView: CardFromReference ref, cardViewState } Nothing -- TODO: discuss
    CardViewAction action -> pure $ Tuple (removeLastCardFilter cif Nothing) action
    ChangeFilter newFilter -> cardsManagerView i newFilter cvs Nothing

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

    currentTitleFilter = case indexFilter of
      TitleFilter t -> t
      _ -> ""

    allSortedTags = sort $ nub $ fold $ (\(CardEntry { tags }) -> tags) <$> entries

    shownEntries = filter (\(CardEntry r) -> archived || (not r.archived)) entries

    shownSortedTags = sort $ nub $ fold $ (\(CardEntry { tags }) -> tags) <$> shownEntries

    getFilterHeader :: forall a. IndexFilter -> Widget HTML a
    getFilterHeader f =
      case f of
        ComposedAndFilter f' f'' -> p' [getFilterHeader f', text " and ", getFilterHeader f'']
        ComposedOrFilter f' f''  -> p' [getFilterHeader f', text " or ", getFilterHeader f'']
        TitleFilter title      -> text title
        SpecificCardFilter ce  -> text "last created card"
        TagFilter tag          -> text tag
        RecentFilter           -> text "recent"
        UntaggedFilter         -> text "untagged"
        NoFilter               -> text "clipperz logo"

    countShownCards :: IndexFilter -> Int
    countShownCards indexFilt = length $ filter (toFilterFunc indexFilt) shownEntries

    getFilterListElement :: IndexFilter -> String -> Widget HTML IndexFilter
    getFilterListElement indexFilt s = clickableListItemWidget false (div [] [ text s, div [] [text $ show $ countShownCards indexFilt]]) [] indexFilt
