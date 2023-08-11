module DataModel.FragmentData where

import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Generic.Rep (class Generic)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Card (Card)
import DataModel.Credentials (Credentials)

data FragmentData = Registration | Login Credentials | AddCard Card | Unrecognized String
instance showFragmentData :: Show FragmentData where
  show Registration            = "Registration"
  show (Login cred)            = ("Login: "       <> show cred)
  show (AddCard card)          = ("AddCard: "     <> show card)
  show (Unrecognized fragment) = ("Urecognized: " <> fragment)
derive instance genericFragmentData :: Generic FragmentData _
instance encodeJsonFragmentData :: EncodeJson FragmentData where
  encodeJson a = genericEncodeJson a
instance decodeJsonFragmentData :: DecodeJson FragmentData where
  decodeJson a = genericDecodeJson a