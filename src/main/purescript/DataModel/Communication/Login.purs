module DataModel.Communication.Login where

import Data.HexString

import DataModel.User (MasterKey)

type LoginStep1Response = { s  :: HexString
                          , bb :: HexString
                          }

type LoginStep2Response = { m2 :: HexString
                          , masterKey :: MasterKey
                          }
