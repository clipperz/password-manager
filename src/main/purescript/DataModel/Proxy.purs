module DataModel.Proxy where

import Control.Applicative (pure)
import Effect.Aff (Aff)

class Backend a where
  doGenericRequest :: a -> Aff String

data Proxy = OnlineProxy String | OfflineProxy

instance proxyBackend :: Backend Proxy where
  doGenericRequest (OnlineProxy url) = pure ""
  doGenericRequest  OfflineProxy     = pure ""
