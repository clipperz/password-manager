module DataModel.User where

import Data.HexString (HexString)

type UserCard = {
  c :: HexString
, v :: HexString
, s :: HexString
, srpVersion :: String
, masterKeyEncodingVersion :: String
, masterKeyContent :: HexString
}