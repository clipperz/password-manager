module Functions.HashCash where

import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, toArrayBuffer, fromArrayBuffer)
import Data.List (List(..), (:))
import Data.String.CodeUnits (take)
import Effect.Aff (Aff)
import Functions.ArrayBuffer (toBitString)
import SRP (HashFunction, randomArrayBuffer)

type Challenge = HexString
type Cost = Int
type Receipt = HexString

computeHashCash :: HashFunction -> Challenge -> Cost -> Aff Receipt
computeHashCash hash challenge cost = do
  -- _ <- log "PING"
  receipt <- fromArrayBuffer <$> randomArrayBuffer 32
  verification <- verifyChallenge hash challenge cost receipt
  case verification of
    true -> pure receipt
    false -> computeHashCash hash challenge cost

verifyChallenge :: HashFunction -> Challenge -> Cost -> Receipt -> Aff Boolean
verifyChallenge hashFunc challenge cost receipt = do
  hash <- hashFunc $ (toArrayBuffer receipt) : Nil
  let challengeBits = toBitString $ toArrayBuffer challenge
  let hashBits      = toBitString hash
  pure $ (take cost challengeBits) == (take cost hashBits)
