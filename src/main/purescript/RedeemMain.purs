module RedeemMain where

import Concur.React.DOM (div, text)
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Data.Array (last)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (hex)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, split)
import Data.Unit (Unit)
import DataModel.AppState (Proxy(..))
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.SRPVersions.SRP (baseSRPConf, hashFuncSHA256)
import Effect (Effect)
import Functions.Communication.Backend (ConnectionState)
import OperationalWidgets.RedeemWidget (redeemWidget)
import Web.HTML (window)
import Web.HTML.Location (hash, pathname)
import Web.HTML.Window (location)

initialConnectionState :: ConnectionState
initialConnectionState = {
  proxy: OnlineProxy "/api" { toll: Loading Nothing, currentChallenge: Nothing } Nothing
, hashFunc: hashFuncSHA256
, srpConf: baseSRPConf
, c: hex ""
, p: hex ""
}

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
      _  ->    (redeemWidget initialConnectionState id key)
