module DataModel.Proxy where

import Data.Semigroup ((<>))
import Data.Show (class Show)

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)

data Proxy = OnlineProxy String | OfflineProxy

derive instance genericProxy :: Generic Proxy _

instance encodeJsonProxy :: EncodeJson Proxy where
  encodeJson a = genericEncodeJson a

instance decodeJsonProxy :: DecodeJson Proxy where
  decodeJson a = genericDecodeJson a

instance showProxy :: Show Proxy where
  show (OnlineProxy s) = "Online Proxy: " <> s
  show (OfflineProxy)  = "Offline Proxy"
