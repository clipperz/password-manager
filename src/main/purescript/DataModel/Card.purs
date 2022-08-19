module DataModel.Card where

import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (rmap)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)

data CardField =
  CardField_v1
    { name   :: String
    , value  :: String
    , locked :: Boolean
    }

instance showCardField :: Show CardField where
  show (CardField_v1 {name: n, value: v, locked: l}) = "[" <> show l <> "] " <> n <> ": " <> v

instance encodeJsonCardField :: EncodeJson CardField where
  encodeJson (CardField_v1 record) = encodeJson record

instance decodeJsonCardField :: DecodeJson CardField where
  decodeJson json = rmap (\record -> CardField_v1 record) (decodeJson json)

type CardValues = {
    title  :: String
  , tags   :: Array String
  , fields :: Array CardField
  -- , attachments :: ?? --TODO
  , notes  :: String
}

type Card = { reference :: String, content :: CardValues }
