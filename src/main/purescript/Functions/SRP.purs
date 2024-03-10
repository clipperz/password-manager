module Functions.SRP where

import Bytes (asArrayBuffer)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT, except)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.BigInt (BigInt, fromInt, mod, modPow, toNumber, toString)
import Data.Decimal as Decimal
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer, hex, toArrayBuffer, fromBigInt)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((>))
import Data.Ring ((-), (*), (+))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import DataModel.SRPVersions.SRP (SRPError(..), SRPConf)
import Effect.Aff (Aff)
import Effect.Fortuna (randomBytes)
import Functions.ArrayBuffer (arrayBufferToBigInt, bigIntToArrayBuffer, xor)

randomArrayBuffer :: Int -> Aff ArrayBuffer
randomArrayBuffer l = do
  rb <- randomBytes l
  pure $ asArrayBuffer rb

-- | Returns a random BigInt on which checks have been run
randomBigInt :: SRPConf -> Aff (Either SRPError BigInt) --TODO refactor
randomBigInt srpConf = do
  maybeRandomBigInt <- arrayBufferToBigInt <$> randomArrayBuffer 32
  case maybeRandomBigInt of
    Just randBigInt -> do
      let maybeDecimalRandBigInt = Decimal.fromString (toString randBigInt)
      let maybeDecimalN = Decimal.fromString (toString srpConf.group.nn)
      let maybeDecimalg = Decimal.fromString (toString srpConf.group.g)
      let check = (\log -> (\a -> a > log) <$> maybeDecimalRandBigInt) :: Decimal.Decimal -> Maybe Boolean
      let computeLogBaseG = (\g -> (\n -> Decimal.log10(n) / Decimal.log10(g)) <$> maybeDecimalN) :: Decimal.Decimal -> Maybe Decimal.Decimal
      let maybeBool = maybeDecimalg >>= computeLogBaseG >>= check :: Maybe Boolean
      case maybeBool of
        Just true  -> pure $ Right randBigInt
        Just false -> randomBigInt srpConf
        Nothing    -> pure $ Left $ SRPError "Generate random number: Error in converting from BigInt to BigDecimal"
    Nothing -> pure $ Left $ SRPError "Generate random number: Error in converting from bytes to BigInt"

prepareC :: SRPConf -> String -> String -> Aff ArrayBuffer
prepareC conf username password = conf.hash $ (toArrayBuffer $ hex username) : (toArrayBuffer $ hex password) : Nil

prepareP :: SRPConf -> String -> String -> Aff ArrayBuffer
prepareP conf username password = conf.hash $ (toArrayBuffer $ hex password): (toArrayBuffer $ hex username) : Nil

computeA :: SRPConf -> BigInt -> BigInt
computeA srpConf a = modPow srpConf.group.g a srpConf.group.nn

prepareA :: SRPConf -> Aff (Either SRPError (Tuple BigInt BigInt))
prepareA srpConf = do
  eithera <- randomBigInt srpConf
  pure $ (\a -> Tuple a (computeA srpConf a)) <$> eithera

computeB :: SRPConf -> BigInt -> BigInt -> BigInt
computeB srpConf b v = mod ((mod (srpConf.k * v) srpConf.group.nn) + (modPow srpConf.group.g b srpConf.group.nn)) srpConf.group.nn

prepareB :: SRPConf -> BigInt -> Aff (Either SRPError (Tuple BigInt BigInt))
prepareB srpConf v = do
  eitherb <- randomBigInt srpConf
  pure $ (\b -> Tuple b (computeB srpConf b v)) <$> eitherb

prepareV :: SRPConf -> ArrayBuffer -> ArrayBuffer -> Aff (Either SRPError HexString)
prepareV srpConf salt p = do
  abx <- srpConf.kdf srpConf.hash salt p
  runExceptT $ do
    bix <- except $ note (SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt abx)
    pure $ fromBigInt (modPow srpConf.group.g bix srpConf.group.nn)

prepareU :: SRPConf -> BigInt -> BigInt -> Aff (Either SRPError BigInt)
prepareU srpConf aa bb = do
  let abA = bigIntToArrayBuffer aa
  let abB = bigIntToArrayBuffer bb
  abu <- srpConf.hash $ abA : abB : Nil
  let u = fromMaybe (fromInt 0) (arrayBufferToBigInt abu) :: BigInt
  pure $ if (toNumber u) == (0.0 :: Number)
    then Left $ SRPError $ "u is equal to 0" <> show u
    else Right u


computeSClient :: SRPConf -> BigInt -> BigInt -> BigInt -> BigInt -> BigInt
computeSClient srpConf bb x a u = 
  let firstOp = mod (bb - (mod (srpConf.k * (modPow srpConf.group.g x srpConf.group.nn)) srpConf.group.nn)) srpConf.group.nn
      secondOp = a + (u * x)
  in modPow firstOp secondOp srpConf.group.nn 
  
prepareSClient :: SRPConf -> BigInt -> BigInt -> BigInt -> BigInt -> Aff (Either SRPError BigInt)
prepareSClient srpConf aa bb x a = do
  u :: Either SRPError BigInt <- prepareU srpConf aa bb
  pure $ (computeSClient srpConf bb x a) <$> u

computeSServer :: SRPConf -> BigInt -> BigInt -> BigInt -> BigInt -> BigInt
computeSServer srpConf aa v b u = modPow (mod (aa * (modPow v u srpConf.group.nn)) srpConf.group.nn) b srpConf.group.nn

prepareSServer :: SRPConf -> BigInt -> BigInt -> BigInt -> BigInt  -> Aff (Either SRPError BigInt)
prepareSServer srpConf aa bb v b = do
  u :: Either SRPError BigInt <- prepareU srpConf aa bb
  pure $ (computeSServer srpConf aa v b) <$> u

prepareK :: SRPConf -> BigInt -> Aff ArrayBuffer
prepareK srpConf ss = srpConf.hash $ (bigIntToArrayBuffer ss) : Nil

prepareM1 :: SRPConf -> HexString -> HexString -> BigInt -> BigInt -> ArrayBuffer -> Aff ArrayBuffer
prepareM1 srpConf c s aa bb kk = do
  hc <- srpConf.hash $ (toArrayBuffer c) : Nil
  hashN <- srpConf.hash $ (bigIntToArrayBuffer srpConf.group.nn) : Nil
  let abg = bigIntToArrayBuffer srpConf.group.g
  hashg <- srpConf.hash $ abg : Nil
  let xorNg = xor hashN hashg
  srpConf.hash $ xorNg : hc : (toArrayBuffer s) : (bigIntToArrayBuffer aa) : (bigIntToArrayBuffer bb) : kk : Nil

prepareM2 :: SRPConf -> BigInt -> ArrayBuffer -> ArrayBuffer -> Aff ArrayBuffer
prepareM2 srpConf aa m1 kk = do
  srpConf.hash $ (bigIntToArrayBuffer aa) : m1 : kk : Nil

checkM1 :: SRPConf -> HexString -> HexString -> BigInt -> BigInt -> ArrayBuffer -> ArrayBuffer -> Aff Boolean
checkM1 srpConf c s aa bb kk m1 = do
  serverM1 <- prepareM1 srpConf c s aa bb kk
  pure $ (fromArrayBuffer serverM1) == (fromArrayBuffer m1)

checkM2 :: SRPConf -> BigInt -> ArrayBuffer -> ArrayBuffer -> ArrayBuffer -> Aff Boolean
checkM2 srpConf aa m1 kk m2 = do
  clientM2 <- prepareM2 srpConf aa m1 kk
  pure $ (fromArrayBuffer clientM2) == (fromArrayBuffer m2) -- TODO: improve
