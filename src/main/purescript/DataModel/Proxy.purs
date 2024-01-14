module DataModel.ProxyType where

import Data.Eq (class Eq)
import Data.HexString (HexString)
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

type BackendSessionRecord = {
  b :: Maybe HexString
, aa :: Maybe HexString
, bb :: Maybe HexString
}
data BackendSessionState = BackendSessionState BackendSessionRecord

derive instance eqBackendSessionState :: Eq BackendSessionState

instance showBackendSessionState :: Show BackendSessionState where
  show (BackendSessionState r) = show r

data ProxyType = OnlineProxy String | OfflineProxy BackendSessionState

derive instance eqProxy :: Eq ProxyType

instance showProxy :: Show ProxyType where
  show (OnlineProxy  s)  = "Online ProxyType: " <> s
  show (OfflineProxy c)  = "Offline ProxyType: " <> (show c)

