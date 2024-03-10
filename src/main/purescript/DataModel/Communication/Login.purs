module DataModel.Communication.Login where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.HexString (HexString, hexStringCodec)
import DataModel.UserVersions.User (MasterKey, masterKeyCodec)

type LoginStep1Response = { s  :: HexString
                          , bb :: HexString
                          }
loginStep1ResponseCodec :: CA.JsonCodec LoginStep1Response
loginStep1ResponseCodec =
  CAR.object "loginStep1Response"
    { s  : hexStringCodec
    , bb : hexStringCodec
    }

type LoginStep2Response = { m2        :: HexString
                          , masterKey :: MasterKey
                          }
loginStep2ResponseCodec :: CA.JsonCodec LoginStep2Response
loginStep2ResponseCodec =
  CAR.object "loginStep2Response"
    { m2        : hexStringCodec
    , masterKey : masterKeyCodec
    }