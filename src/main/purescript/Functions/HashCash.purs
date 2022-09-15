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
import Effect.Class.Console (log)
import Functions.ArrayBuffer (toBitString)
import SRP (HashFunction, randomArrayBuffer)

type Toll = HexString
type Cost = Int
type Receipt = HexString

type TollChallenge = {toll :: Toll, cost :: Cost}

computeReceipt :: HashFunction -> TollChallenge -> Aff Receipt
computeReceipt hash challenge = do
  _ <- log "PING"
  receipt <- fromArrayBuffer <$> randomArrayBuffer 32
  verification <- verifyReceipt hash challenge receipt
  case verification of
    true -> pure receipt
    false -> computeReceipt hash challenge

verifyReceipt :: HashFunction -> TollChallenge -> Receipt -> Aff Boolean
verifyReceipt hashFunc {toll, cost} receipt = do
  hash <- hashFunc $ (toArrayBuffer receipt) : Nil
  let tollBits = toBitString $ toArrayBuffer toll
  let hashBits      = toBitString hash
  pure $ (take cost tollBits) == (take cost hashBits)
