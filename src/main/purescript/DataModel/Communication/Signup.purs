module DataModel.Communication.Signup where

import Data.HexString (HexString)
import Data.Tuple (Tuple)
import DataModel.User (RequestUserCard)

type RegisterUserRequest = {
  user                 :: RequestUserCard
, p                    :: HexString
, preferencesReference :: HexString
, preferencesContent   :: HexString
, indexCardReference   :: HexString
, indexCardContent     :: HexString
, cards                :: Array (Tuple HexString HexString)
}