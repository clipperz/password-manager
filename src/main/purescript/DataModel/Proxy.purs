module DataModel.Proxy where

import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader as RE
import Affjax.ResponseFormat as RF
import Affjax.Web as AXW
import Control.Applicative (pure)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)

data Proxy = OnlineProxy String | OfflineProxy

instance showProxy :: Show Proxy where
  show (OnlineProxy s) = "Online Proxy: " <> s
  show (OfflineProxy)  = "Offline Proxy"
