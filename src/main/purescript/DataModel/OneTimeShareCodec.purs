module DataModel.OneTimeShareCodec where

import Control.Bind ((>>=))
import Control.Category ((<<<))
import Data.Codec.Argonaut (JsonDecodeError(..), decode, encode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Profunctor (wrapIso)
import Data.Time.Duration (Milliseconds(..))
import DataModel.Codec as Codec
import DataModel.Communication.OneTimeShare (SecretRequestData, SecretVersion(..))

secretRequestDataCodec :: CA.JsonCodec SecretRequestData
secretRequestDataCodec = 
  CAR.object "secretData"
    { secret   : Codec.hexStringCodec
    , version  : secretVersionCodec
    , duration : wrapIso Milliseconds CA.number
    }

secretVersionCodec :: CA.JsonCodec SecretVersion
secretVersionCodec = CA.codec' (\s -> decode CA.string s >>= secretVersionFromString) (encode CA.string <<< secretVersionToString)
  where
    secretVersionToString :: SecretVersion -> String
    secretVersionToString V_1 = "V_1"

    secretVersionFromString :: String -> Either JsonDecodeError SecretVersion
    secretVersionFromString string =
      case string of
        "V_1" -> Right V_1
        _     -> Left (TypeMismatch string)