module DataModel.Proxy where

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

-- import Data.Map (Map)

data BackendSessionState = BackendSessionState {
  b :: Maybe HexString
, aa :: Maybe HexString
}

derive instance eqBackendSessionState :: Eq BackendSessionState
derive instance genericBackendSessionState :: Generic BackendSessionState _

instance encodeJsonBackendSessionState :: EncodeJson BackendSessionState where
  encodeJson a = genericEncodeJson a

instance decodeJsonBackendSessionState :: DecodeJson BackendSessionState where
  decodeJson a = genericDecodeJson a

instance showBackendSessionState :: Show BackendSessionState where
  show (BackendSessionState r) = show r

data Proxy = OnlineProxy String | OfflineProxy BackendSessionState

derive instance eqProxy :: Eq Proxy
derive instance genericProxy :: Generic Proxy _

instance encodeJsonProxy :: EncodeJson Proxy where
  encodeJson a = genericEncodeJson a

instance decodeJsonProxy :: DecodeJson Proxy where
  decodeJson a = genericDecodeJson a

instance showProxy :: Show Proxy where
  show (OnlineProxy  s)  = "Online Proxy: " <> s
  show (OfflineProxy c)  = "Offline Proxy: " <> (show c)

