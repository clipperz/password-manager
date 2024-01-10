module DataModel.ProxyType where

import Data.Eq (class Eq)
import Data.HexString (HexString)
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

type BackendSessionRecord = {
  b :: Maybe HexString
, aa :: Maybe HexString
, bb :: Maybe HexString
}
data BackendSessionState = BackendSessionState BackendSessionRecord

derive instance eqBackendSessionState :: Eq BackendSessionState
derive instance genericBackendSessionState :: Generic BackendSessionState _

instance encodeJsonBackendSessionState :: EncodeJson BackendSessionState where
  encodeJson a = genericEncodeJson a

instance decodeJsonBackendSessionState :: DecodeJson BackendSessionState where
  decodeJson a = genericDecodeJson a

instance showBackendSessionState :: Show BackendSessionState where
  show (BackendSessionState r) = show r

data ProxyType = OnlineProxy String | OfflineProxy BackendSessionState

derive instance eqProxy :: Eq ProxyType
derive instance genericProxy :: Generic ProxyType _

instance encodeJsonProxy :: EncodeJson ProxyType where
  encodeJson a = genericEncodeJson a

instance decodeJsonProxy :: DecodeJson ProxyType where
  decodeJson a = genericDecodeJson a

instance showProxy :: Show ProxyType where
  show (OnlineProxy  s)  = "Online ProxyType: " <> s
  show (OfflineProxy c)  = "Offline ProxyType: " <> (show c)

