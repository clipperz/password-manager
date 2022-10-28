module Views.IndexView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (ol, text)
import Data.Array (fromFoldable)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.List (sort)
import DataModel.Index (Index(..), CardEntry(..))
import Views.SimpleWebComponents (clickableListItemWidget)

indexView :: Index -> Widget HTML CardEntry
indexView (Index cards) = do
  let sortedCards = fromFoldable $ sort cards :: Array CardEntry
  ol []
    ((\entry@(CardEntry { title, archived }) -> 
      clickableListItemWidget false (text title) (if archived then ["archived"] else []) entry
     ) <$> sortedCards)
