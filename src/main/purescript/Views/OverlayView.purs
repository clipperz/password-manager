module Views.OverlayView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, span, text)
import Concur.React.Props as Props
import Data.Array (range)
import Data.Formatter.Number (Formatter(..), format)
import Data.Function (($))
import Data.Functor (map, (<$>))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))

data OverlayStatus = Hidden | Spinner | Done | Failed | Copy
type OverlayInfo = { status :: OverlayStatus, message :: String }

overlay :: forall a. OverlayInfo -> Widget HTML a
overlay info =
  div [Props.classList (Just <$> ["overlay", visibility])] [
    case info.status of
      Hidden  ->  div  [] []
      Spinner ->  div  [Props.className "spinner"]     $ map (\i -> div [Props.className ("bar" <> (format dd (toNumber i)))] []) (range 1 12)
      Done    ->  span [Props.className "icon done"]   [text "done"]
      Copy    ->  span [Props.className "icon copy"]   [text "copy"]
      Failed  ->  span [Props.className "icon failed"] [text "failed"]
    ,
    span [Props.className "title"] [text info.message]
  ]
  where
    dd :: Formatter
    dd  = Formatter { comma: false, before: 2, after: 0, abbreviations: false, sign: false }

    visibility :: String
    visibility = case info.status of
      Hidden  -> "hidden"
      _       -> "visible"