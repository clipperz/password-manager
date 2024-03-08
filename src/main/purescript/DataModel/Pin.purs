module DataModel.Pin where

import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR

type PasswordPin = { padding :: Int, passphrase :: String }
passwordPinCodec :: CA.JsonCodec PasswordPin
passwordPinCodec =
  CAR.object "passwordPin" { padding : CA.int , passphrase : CA.string }
