module DataModel.User where

import Data.HexString (HexString)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Bifunctor (rmap)

newtype UserCard =
  UserCard
    { c :: HexString
    , v :: HexString
    , s :: HexString
    , srpVersion :: String
    , masterKeyEncodingVersion :: String
    , masterKeyContent :: HexString
    }

instance encodeJsonUserCard :: EncodeJson UserCard where
  encodeJson (UserCard record) = encodeJson record

instance decodeJsonUserCard :: DecodeJson UserCard where
  decodeJson json = rmap (\record -> UserCard record) (decodeJson json)

newtype IndexReference =
  IndexReference
    { reference :: HexString
    , masterKey :: HexString
    , indexVersion :: String
    }

instance encodeJsonIndexReference :: EncodeJson IndexReference where
  encodeJson (IndexReference record) = encodeJson record

instance decodeJsonIndexReference :: DecodeJson IndexReference where
  decodeJson json = rmap (\record -> IndexReference record) (decodeJson json)