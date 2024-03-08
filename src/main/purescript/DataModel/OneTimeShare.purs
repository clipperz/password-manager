module DataModel.OneTimeShare where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Codec.Argonaut.Variant as CAV
import Data.Either (Either(..))
import Data.Function (($))
import Data.HexString (HexString, hexStringCodec)
import Data.Profunctor (dimap, wrapIso)
import Data.Time.Duration (Milliseconds(..), Seconds)
import Data.Unit (unit)
import Data.Variant as V
import Type.Proxy (Proxy(..))

data SecretVersion = SecretVersion_1
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

type SecretRequestData = { secret   :: HexString
                         , version  :: SecretVersion
                         , duration :: Milliseconds
                         }
secretRequestDataCodec :: CA.JsonCodec SecretRequestData
secretRequestDataCodec = 
  CAR.object "secretData"
    { secret   : hexStringCodec
    , version  : secretVersionCodec
    , duration : wrapIso Milliseconds CA.number
    }

type SecretData = { secret   :: String
                  , pin      :: String
                  , duration :: Seconds
                  }