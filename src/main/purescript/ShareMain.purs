module ShareMain where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.Run (runWidgetInDom)
import Control.Bind (bind, (>>=))
import Control.Semigroupoid ((<<<))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (Base(..), hex, toString)
import Data.Maybe (Maybe(..))
import Data.String (drop)
import Data.Unit (Unit)
import DataModel.AppState (HashState(..))
import DataModel.AsyncValue (AsyncValue(..))
import DataModel.SRP (hashFuncSHA256)
import Effect (Effect)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Functions.Communication.StatelessBackend (ConnectionState, Proxy(..))
import Functions.JSState (saveAppState)
import Functions.State (computeInitialState)
import OperationalWidgets.ShareWidget (shareWidget)
import Views.ShareView (Secret(..))
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..), replaceState)
import Web.HTML.Location (hash, pathname)
import Web.HTML.Window (history, location)

wrapper :: forall a. Widget HTML a -> Widget HTML a
wrapper widget = do
  _ <- liftEffect $ computeInitialState >>= saveAppState
  widget

initialConnectionState :: ConnectionState
initialConnectionState = {
  proxy: OnlineProxy "/api" { toll: Loading Nothing, currentChallenge: Nothing } Nothing
, hashFunc: hashFuncSHA256
}

main :: Effect Unit
main = do
  l <- window >>= location
  secret <- (toString Dec <<< hex <<< drop 1) <$> hash l
  pathName <- pathname l
  _ <- window >>= history >>= replaceState (unsafeToForeign {}) (DocumentTitle "") (URL pathName)
  runWidgetInDom "share" (shareWidget initialConnectionState $ SecretString secret )
