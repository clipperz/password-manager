module DataModel.SRPCodec where

import Control.Bind ((>>=))
import Control.Category ((<<<))
import Data.Codec.Argonaut (JsonDecodeError(..), decode, encode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Function (($))
import Data.HexString (HexString)
import Data.Profunctor (wrapIso)
import DataModel.Codec as Codec
import DataModel.Communication.Login (LoginStep2Response, LoginStep1Response)
import DataModel.Communication.Signup (RegisterUserRequest)
import DataModel.User (MasterKey, MasterKeyEncodingVersion(..), RequestUserCard(..), SRPVersion(..), UserCard(..))

masterKeyEncodingVersionCodec :: CA.JsonCodec MasterKeyEncodingVersion
masterKeyEncodingVersionCodec = CA.codec' (\s -> decode CA.string s >>= masterKeyEncodingVersionFromString) (encode CA.string <<< masterKeyEncodingVersionToString)
  where
    masterKeyEncodingVersionToString :: MasterKeyEncodingVersion -> String
    masterKeyEncodingVersionToString V_1 = "1.0"

    masterKeyEncodingVersionFromString :: String -> Either JsonDecodeError MasterKeyEncodingVersion
    masterKeyEncodingVersionFromString string =
      case string of
        "1.0" -> Right V_1
        _     -> Left (TypeMismatch string)

srpVersionCodec :: CA.JsonCodec SRPVersion
srpVersionCodec = CA.codec' (\s -> decode CA.string s >>= srpVersionFromString) (encode CA.string <<< srpVersionToString)
  where
    srpVersionToString :: SRPVersion -> String
    srpVersionToString V_6a = "6a"

    srpVersionFromString :: String -> Either JsonDecodeError SRPVersion
    srpVersionFromString string =
      case string of
        "6a" -> Right V_6a
        _    -> Left (TypeMismatch string)


masterKeyCodec :: CA.JsonCodec MasterKey
masterKeyCodec = CAC.tuple Codec.hexStringCodec masterKeyEncodingVersionCodec

requestUserCardCodec :: CA.JsonCodec RequestUserCard
requestUserCardCodec = wrapIso RequestUserCard $
  CAR.object "requestUserCard"
    { c : Codec.hexStringCodec
    , v : Codec.hexStringCodec
    , s : Codec.hexStringCodec
    , srpVersion: srpVersionCodec
    , originMasterKey: CAR.optional Codec.hexStringCodec
    , masterKey: masterKeyCodec
    }

loginStep1RequestCodec :: CA.JsonCodec {c :: HexString, aa :: HexString}
loginStep1RequestCodec = 
  CAR.object "loginStep1Request" 
    { c:  Codec.hexStringCodec
    , aa: Codec.hexStringCodec
    }

loginStep1ResponseCodec :: CA.JsonCodec LoginStep1Response
loginStep1ResponseCodec =
  CAR.object "loginStep1Response"
    { s  : Codec.hexStringCodec
    , bb : Codec.hexStringCodec
    }

loginStep2RequestCodec :: CA.JsonCodec {m1 :: HexString}
loginStep2RequestCodec = 
  CAR.object "loginStep2Request"
    { m1: Codec.hexStringCodec
    }

loginStep2ResponseCodec :: CA.JsonCodec LoginStep2Response
loginStep2ResponseCodec =
  CAR.object "loginStep2Response"
    { m2        : Codec.hexStringCodec
    , masterKey : masterKeyCodec
    }

registerUserRequestCodec :: CA.JsonCodec RegisterUserRequest
registerUserRequestCodec =
  CAR.object "registerUserRequest"
    { user                 : requestUserCardCodec
    , p                    : Codec.hexStringCodec
    , userInfoReference    : Codec.hexStringCodec
    , userInfoContent      : Codec.hexStringCodec
    , userInfoIdentifier   : Codec.hexStringCodec
    , indexCardReference   : Codec.hexStringCodec
    , indexCardContent     : Codec.hexStringCodec
    , indexCardIdentifier  : Codec.hexStringCodec
    , cards                : CA.array (CAR.object "card" {cardContent: Codec.hexStringCodec, cardReference: Codec.hexStringCodec, cardIdentifier: Codec.hexStringCodec})
    }

userCardCodec :: CA.JsonCodec UserCard
userCardCodec = wrapIso UserCard $
  CAR.object "userCard"
    { originMasterKey : Codec.hexStringCodec
    , masterKey       : masterKeyCodec
    }