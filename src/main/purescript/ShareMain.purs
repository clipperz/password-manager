module ShareMain where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.Run (runWidgetInDom)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except (runExceptT)
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)
import Data.Show (show)
import Data.String (Pattern(..), drop, split)
import Data.Unit (Unit, unit)
import DataModel.Card (Card)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.JSState (modifyAppState)
import Functions.State (computeInitialState)
import JSURI (decodeURI)
import OperationalWidgets.RedeemWidget (redeemWidget)
import OperationalWidgets.ShareWidget (shareWidget)
import Views.ShareView (Secret(..))
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
  secret <- drop 1 <$> hash l
  runWidgetInDom "share" ( wrapper $ shareWidget $ 
    case fromJsonString $ fromMaybe secret (decodeURI secret) of
      Right (_ :: Card)  -> SecretCard secret
      Left _             -> case secret of 
        "" -> NoSecret
        _  -> SecretString secret
)
