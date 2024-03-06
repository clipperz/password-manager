module DataModel.OneTimeShareCodec where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Function (($))
import Data.Profunctor (dimap, wrapIso)
import Data.Time.Duration (Milliseconds(..))
import Data.Unit (unit)
import Data.Variant as V
import DataModel.Codec as Codec
import DataModel.Communication.OneTimeShare (SecretRequestData, SecretVersion(..))
import Type.Proxy (Proxy(..))

secretRequestDataCodec :: CA.JsonCodec SecretRequestData
secretRequestDataCodec = 
  CAR.object "secretData"
    { secret   : Codec.hexStringCodec
    , version  : secretVersionCodec
    , duration : wrapIso Milliseconds CA.number
    }

secretVersionCodec :: CA.JsonCodec SecretVersion
secretVersionCodec = dimap toVariant fromVariant $ CAV.variantMatch
    { secretVersion_1: Left unit
    }
  where
    toVariant = case _ of
      SecretVersion_1 -> V.inj (Proxy :: _ "secretVersion_1") unit
    fromVariant = V.match
      { secretVersion_1: \_ -> SecretVersion_1
      }