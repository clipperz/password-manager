module RedeemMain where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Data.Array (last)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, split)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class (liftEffect)
import Functions.JSState (modifyAppState)
import Functions.State (computeInitialState)
import OperationalWidgets.RedeemWidget (redeemWidget)
import Web.HTML (window)
import Web.HTML.Location (hash, pathname)
import Web.HTML.Window (location)

wrapper :: forall a. Widget HTML a -> Widget HTML a
wrapper widget = do
  _ <- liftEffect $ computeInitialState >>= modifyAppState
  widget

main :: Effect Unit
main = do
  l <- window >>= location
  pathName <- pathname l
  key <- drop 1 <$> hash l
  _id <- pure $ last (split (Pattern "/") pathName)
  runWidgetInDom "redeem" $ case _id of
    Nothing -> (div [Props.className "error"] [text "Missing document id"])
    Just "" -> (div [Props.className "error"] [text "Missing document id"])
    Just id -> case key of
      "" ->    (div [Props.className "error"] [text "Missing document encryption key"])
      _  ->    (wrapper $ redeemWidget id key)
