module DataModel.Proxy where

import Control.Applicative (pure)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Effect.Aff (Aff)

class Backend a where
  doGenericRequest :: a -> Aff String

data Proxy = OnlineProxy String | OfflineProxy

instance showProxy :: Show Proxy where
  show (OnlineProxy s) = "Online Proxy: " <> s
  show (OfflineProxy)  = "Offline Proxy"

instance proxyBackend :: Backend Proxy where
  doGenericRequest (OnlineProxy url) = pure ""
  doGenericRequest  OfflineProxy     = pure ""
