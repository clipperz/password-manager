module RedeemMain where

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
import Functions.JSState (modifyAppState)
import Functions.State (computeInitialState)
import OperationalWidgets.RedeemWidget (redeemWidget)
import Web.HTML (window)
import Web.HTML.Location (hash, setHash)
import Web.HTML.Window (location)

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
  idPayload <- drop 1 <$> hash l
  _ <- setHash "" l
  runWidgetInDom "redeem" ( wrapper $ redeemWidget idPayload )
