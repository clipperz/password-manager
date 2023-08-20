module ShareMain where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.Run (runWidgetInDom)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (show)
import Data.String (drop)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign (unsafeToForeign)
import Functions.JSState (modifyAppState)
import Functions.State (computeInitialState)
import OperationalWidgets.ShareWidget (shareWidget)
import Views.ShareView (Secret(..))
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
  secret <- drop 1 <$> hash l
  pathName <- pathname l
  _ <- window >>= history >>= replaceState (unsafeToForeign {}) (DocumentTitle "") (URL pathName)
  runWidgetInDom "share" ( wrapper $ shareWidget $ SecretString secret )
