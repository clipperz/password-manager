module ShareMain where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text)
import Concur.React.Run (runWidgetInDom)
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String (Pattern(..), split)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.JSState (modifyAppState)
import Functions.State (computeInitialState)
import OperationalWidgets.ShareWidget (shareWidget)
import Web.HTML (window)
import Web.HTML.Location (hash)
import Web.HTML.Window (location)

wrapper :: forall a. Widget HTML a -> Widget HTML a
wrapper widget = do
  initialState <- liftEffect $ runExceptT $ computeInitialState
  case initialState of
    Right st -> liftAff $ do
      modifyAppState st
    Left err -> do
      liftEffect $ log $ show err
  widget

main :: Effect Unit
main = do


  l <- window >>= location
  fragment <- hash l
  runWidgetInDom "share" ( wrapper $ case split (Pattern "=") fragment of
    [ "#share", secret ]     -> shareWidget (Just secret)
    [ "#redeem", idPayload ] -> text ("REDEEM:" <> idPayload )
    _                        -> shareWidget Nothing
  )