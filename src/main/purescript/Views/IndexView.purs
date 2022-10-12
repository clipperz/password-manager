module Views.IndexView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (ol, text)
import Concur.React.Props as Props
import Data.Array (fromFoldable)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (sort)
import DataModel.Index (Index(..), CardEntry(..))
import Views.SimpleWebComponents (clickableListItemWidget)

indexView :: Boolean -> Index -> Widget HTML CardEntry
indexView disabled (Index_v1 cards) = do
  let sortedCards = fromFoldable $ sort cards :: Array CardEntry
  ol [Props.className (if disabled then "disabled" else "")] $ (\entry@(CardEntry_v1 { title, cardReference }) -> clickableListItemWidget disabled (text title) entry) <$> sortedCards
