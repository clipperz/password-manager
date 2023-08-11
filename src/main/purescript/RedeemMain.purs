module RedeemMain where

import Prelude

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text)
import Concur.React.Run (runWidgetInDom)
import Control.Monad.Except (runExceptT)
import Data.Array (last)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), drop, split)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (unsafeToForeign)
import Functions.JSState (modifyAppState)
import Functions.State (computeInitialState)
import OperationalWidgets.RedeemWidget (redeemWidget)
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..), replaceState)
import Web.HTML.Location (hash, pathname)
import Web.HTML.Window (history, location)

wrapper :: forall a. Widget HTML a -> Widget HTML a
wrapper widget = do
  initialState <- liftEffect $ runExceptT $ computeInitialState
  case initialState of
    Right st -> liftAff $ do
      liftEffect $ modifyAppState st
    Left err -> do
      liftEffect $ log $ show err
  widget

main :: Effect Unit
main = do
  l <- window >>= location
  pathName <- pathname l
  key <- drop 1 <$> hash l
  _id <- pure $ last (split (Pattern "/") pathName)
  _ <- log $ show key <> " " <> show _id
  _ <- window >>= history >>= replaceState (unsafeToForeign {}) (DocumentTitle "") (URL pathName)
  case _id of
    Nothing -> runWidgetInDom "redeem" (text "Missing or wrong fragment")
    Just id -> runWidgetInDom "redeem" (wrapper $ redeemWidget id key)  
