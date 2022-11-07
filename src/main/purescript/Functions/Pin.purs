module Functions.Pin where

import Control.Bind (bind)
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.BigInt (fromInt)
import Data.Function (($))
import Data.List (List(..), (:))
import DataModel.SRP (HashFunction)
import Effect.Aff (Aff)
import Functions.ArrayBuffer (bigIntToArrayBuffer)
import Functions.State (getHashFunctionFromAppState)

generateKeyFromPin :: HashFunction -> Int -> Aff CryptoKey
generateKeyFromPin hashf pin = do
  pinBuffer <- hashf $ (bigIntToArrayBuffer $ fromInt pin) : Nil
  KI.importKey raw pinBuffer (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
