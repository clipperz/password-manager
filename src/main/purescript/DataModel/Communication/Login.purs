module DataModel.Communication.Login where

import Data.HexString

type LoginStep1Response = { s  :: HexString
                          , bb :: HexString
                          }

type LoginStep2Response = { m2 :: HexString
                          , encIndexReference :: HexString
                          }
