module Widgets.IndexCard where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (ol', text)
import Data.Array (fromFoldable)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (sort)
import Data.Map.Internal (values)

import DataModel.IndexCard (IndexCard(..), CardIndex(..), CardReference)
import Widgets.SimpleWebComponents (clickableListItemWidget)

indexCard :: IndexCard -> Widget HTML CardReference
indexCard (IndexCard_v1 {cards: cards}) = do
  let sortedCards = fromFoldable $ sort $ values cards :: Array CardIndex
  ol' $ (\(CardIndex_v1 {title: title, cardReference: reference, archived: _}) -> clickableListItemWidget (text title) reference) <$> sortedCards
