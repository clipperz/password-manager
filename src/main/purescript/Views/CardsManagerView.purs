module Views.CardsManagerView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, ol)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Semigroupoid ((<<<))
import Control.Bind (bind)
import Data.Array (nub, sort)
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.List (fold, filter, length)
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
import Views.CardViews (cardView)
import Views.CreateCardView (createCardView)
import Views.IndexView (indexView, IndexFilter(..), toFilterFunc)
import Views.SimpleWebComponents (simpleButton, loadingDiv, simpleTextInputWidgetWithFocus, clickableListItemWidget)
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

data InternalAction = CardViewAction CardViewAction | ChangeFilter IndexFilter

cardsManagerView :: Index -> IndexFilter -> CardViewState -> Maybe AppError -> Widget HTML (Tuple IndexFilter CardViewAction)
cardsManagerView i@(Index entries) indexFilter cvs@{ cardView: _, cardViewState } error = do 
  res <- div [Props._id "cardsManager"] $ (text <$> (fromMaybe $ prettyShow <$> error)) <> [
    div [Props._id "filterView"] [
      ol [][
        getFilterListElement NoFilter "All"
      , getFilterListElement RecentFilter "Recent (TODO)"
      , getFilterListElement UntaggedFilter "Untagged"
      ]
    , (ChangeFilter <<< TitleFilter) <$> simpleTextInputWidgetWithFocus "titleFilter" (text "Title") currentTitleFilter
    , div [] [
      text "Tags"
      ,  ol [Props._id "tagFilter"] ((\tag -> getFilterListElement (TagFilter tag) tag) <$> allSortedTags)
      ]
    ]
  , div [] [
      div [Props._id "filterHeader"] [ getFilterHeader ]
    , div [Props._id "mainView" ] [
        div [Props._id "indexView"] [
          (CardViewAction <<< ShowCard) <$> indexView i indexFilter
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
    CardViewAction (ShowCard ref) -> cardsManagerView i indexFilter { cardView: CardFromReference ref, cardViewState } Nothing -- TODO: discuss
    CardViewAction action -> pure $ Tuple indexFilter action
    ChangeFilter newFilter -> cardsManagerView i newFilter cvs Nothing

  where
    currentTitleFilter = case indexFilter of
      TitleFilter t -> t
      _ -> ""

    allSortedTags = sort $ nub $ fold $ (\(CardEntry { tags }) -> tags) <$> entries

    getFilterHeader =
      case indexFilter of
        TitleFilter title -> text title
        TagFilter tag     -> text tag
        RecentFilter      -> text "recent"
        UntaggedFilter    -> text "untagged"
        NoFilter          -> text "clipperz logo"

    countCards :: IndexFilter -> Int
    countCards indexFilt = length $ filter (toFilterFunc indexFilt) entries

    getFilterListElement :: IndexFilter -> String -> Widget HTML InternalAction
    getFilterListElement indexFilt s = clickableListItemWidget false (div [] [ text s, div [] [text $ show $ countCards indexFilt]]) [] (ChangeFilter indexFilt)