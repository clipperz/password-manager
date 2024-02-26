module DataModel.Communication.Signup where

import Data.HexString (HexString)
import DataModel.User (RequestUserCard)

type RegisterUserRequest = {
  user                :: RequestUserCard
, p                   :: HexString
, userInfoReference   :: HexString
, userInfoContent     :: HexString
, userInfoIdentifier  :: HexString
, indexCardReference  :: HexString
, indexCardContent    :: HexString
, indexCardIdentifier :: HexString
, cards               :: Array {cardReference :: HexString, cardContent :: HexString, cardIdentifier :: HexString}
}