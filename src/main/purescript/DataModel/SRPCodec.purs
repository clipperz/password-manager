module DataModel.SRPCodec where

import Control.Bind ((>>=))
import Control.Category ((<<<))
import Data.Codec.Argonaut (JsonDecodeError(..), decode, encode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Function (($))
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

loginStep2ResponseCodec :: CA.JsonCodec LoginStep2Response
loginStep2ResponseCodec =
  CAR.object "loginStep2Response"
    { m2        : Codec.hexStringCodec
    , masterKey : masterKeyCodec
    }

loginStep1ResponseCodec :: CA.JsonCodec LoginStep1Response
loginStep1ResponseCodec =
  CAR.object "loginStep1Response"
    { s  : Codec.hexStringCodec
    , bb : Codec.hexStringCodec
    }

registerUserRequestCodec :: CA.JsonCodec RegisterUserRequest
registerUserRequestCodec =
  CAR.object "registerUserRequest"
    { user                 : requestUserCardCodec
    , p                    : Codec.hexStringCodec
    , preferencesReference : Codec.hexStringCodec
    , preferencesContent   : Codec.hexStringCodec
    , indexCardReference   : Codec.hexStringCodec
    , indexCardContent     : Codec.hexStringCodec
    , cards                : CA.array (CAC.tuple Codec.hexStringCodec Codec.hexStringCodec)
    }

userCardCodec :: CA.JsonCodec UserCard
userCardCodec = wrapIso UserCard $
  CAR.object "userCard"
    { originMasterKey : Codec.hexStringCodec
    , masterKey       : masterKeyCodec
    }