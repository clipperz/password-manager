module DataModel.Communication.OneTimeShare where

import Data.HexString (HexString)
import Data.Time.Duration (Milliseconds, Seconds)

data SecretVersion = V_1

type SecretRequestData = { secret   :: HexString
                         , version  :: SecretVersion
                         , duration :: Milliseconds
                         }

type SecretData = { secret   :: String
                  , pin      :: String
                  , duration :: Seconds
                  }