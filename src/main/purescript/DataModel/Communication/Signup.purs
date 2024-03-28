module DataModel.Communication.Signup where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.HexString (HexString, hexStringCodec)
import DataModel.UserVersions.User (RequestUserCard, requestUserCardCodec)

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
registerUserRequestCodec :: CA.JsonCodec RegisterUserRequest
registerUserRequestCodec =
  CAR.object "registerUserRequest"
    { user                 : requestUserCardCodec
    , p                    : hexStringCodec
    , userInfoReference    : hexStringCodec
    , userInfoContent      : hexStringCodec
    , userInfoIdentifier   : hexStringCodec
    , indexCardReference   : hexStringCodec
    , indexCardContent     : hexStringCodec
    , indexCardIdentifier  : hexStringCodec
    , cards                : CA.array (CAR.object "card" {cardContent: hexStringCodec, cardReference: hexStringCodec, cardIdentifier: hexStringCodec})
    }