module SRP where

import Bytes (asArrayBuffer)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT, except)
import Crypto.Subtle.Hash (digest, sha256, sha1, HashingFunction)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.BigInt (BigInt, fromInt, mod, modPow, toNumber, toString)
import Data.Decimal as Decimal
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer, hex, toArrayBuffer, toBigInt, fromBigInt)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((>))
import Data.Ring ((-), (*), (+))
import Data.Show (class Show, show)
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Fortuna (randomBytes)
import Functions.ArrayBuffer (concatArrayBuffers, arrayBufferToBigInt, bigIntToArrayBuffer, xor)

data SRPError = SRPError String

instance showSRPError :: Show SRPError where
  show (SRPError s) = show s

bigInt0 :: BigInt
bigInt0 = fromInt 0

type HashFunction = List ArrayBuffer -> Aff ArrayBuffer
type KDF = ArrayBuffer -> ArrayBuffer -> Aff ArrayBuffer
type Group = { nn :: BigInt, g :: BigInt }
type SRPConf = { group :: Group, k :: BigInt, hash :: HashFunction, kdf :: KDF }

getPrepareX :: HashFunction -> KDF
getPrepareX hashFunc = \salt password -> hashFunc $ salt : password : Nil 

group1024 :: Group
group1024 = { nn: (fromMaybe bigInt0 (toBigInt n))
            , g: (fromInt 2)
            }
  where n = hex $  "EEAF0AB9 ADB38DD6 9C33F80A FA8FC5E8 60726187 75FF3C0B 9EA2314C"
                <> "9C256576 D674DF74 96EA81D3 383B4813 D692C6E0 E0D5D8E2 50B98BE4"
                <> "8E495C1D 6089DAD1 5DC7D7B4 6154D6B6 CE8EF4AD 69B15D49 82559B29"
                <> "7BCF1885 C529F566 660E57EC 68EDBC3C 05726CC0 2FD4CBF4 976EAA9A"
                <> "FD5138FE 8376435B 9FC61D2F C0EB06E3"

group2048 :: Group
group2048 = { nn: (fromMaybe bigInt0 (toBigInt n))
            , g: (fromInt 2)}
  where n = hex $  "AC6BDB41 324A9A9B F166DE5E 1389582F AF72B665 1987EE07 FC319294"
                <> "3DB56050 A37329CB B4A099ED 8193E075 7767A13D D52312AB 4B03310D"
                <> "CD7F48A9 DA04FD50 E8083969 EDB767B0 CF609517 9A163AB3 661A05FB"
                <> "D5FAAAE8 2918A996 2F0B93B8 55F97993 EC975EEA A80D740A DBF4FF74"
                <> "7359D041 D5C33EA7 1D281E44 6B14773B CA97B43A 23FB8016 76BD207A"
                <> "436C6481 F1D2B907 8717461A 5B9D32E6 88F87748 544523B5 24B0D57D"
                <> "5EA77A27 75D2ECFA 032CFBDB F52FB378 61602790 04E57AE6 AF874E73"
                <> "03CE5329 9CCC041C 7BC308D8 2A5698F3 A8D0C382 71AE35F8 E9DBFBB6"
                <> "94B5C803 D89F7AE4 35DE236D 525F5475 9B65E372 FCD68EF2 0FA7111F"
                <> "9E4AFF73"

group4096 :: Group
group4096 = { nn: (fromMaybe bigInt0 (toBigInt n))
            , g: (fromInt 5)}
  where n = hex $  "FFFFFFFF FFFFFFFF C90FDAA2 2168C234 C4C6628B 80DC1CD1 29024E08"
                <> "8A67CC74 020BBEA6 3B139B22 514A0879 8E3404DD EF9519B3 CD3A431B"
                <> "302B0A6D F25F1437 4FE1356D 6D51C245 E485B576 625E7EC6 F44C42E9"
                <> "A637ED6B 0BFF5CB6 F406B7ED EE386BFB 5A899FA5 AE9F2411 7C4B1FE6"
                <> "49286651 ECE45B3D C2007CB8 A163BF05 98DA4836 1C55D39A 69163FA8"
                <> "FD24CF5F 83655D23 DCA3AD96 1C62F356 208552BB 9ED52907 7096966D"
                <> "670C354E 4ABC9804 F1746C08 CA18217C 32905E46 2E36CE3B E39E772C"
                <> "180E8603 9B2783A2 EC07A28F B5C55DF0 6F4C52C9 DE2BCBF6 95581718"
                <> "3995497C EA956AE5 15D22618 98FA0510 15728E5A 8AAAC42D AD33170D"
                <> "04507A33 A85521AB DF1CBA64 ECFB8504 58DBEF0A 8AEA7157 5D060C7D"
                <> "B3970F85 A6E1E4C7 ABF5AE8C DB0933D7 1E8C94E0 4A25619D CEE3D226"
                <> "1AD2EE6B F12FFA06 D98A0864 D8760273 3EC86A64 521F2B18 177B200C"
                <> "BBE11757 7A615D6C 770988C0 BAD946E2 08E24FA0 74E5AB31 43DB5BFC"
                <> "E0FD108E 4B82D120 A9210801 1A723C12 A787E6D7 88719A10 BDBA5B26"
                <> "99C32718 6AF4E23C 1A946834 B6150BDA 2583E9CA 2AD44CE8 DBBBC2DB"
                <> "04DE8EF9 2E8EFC14 1FBECAA6 287C5947 4E6BC05D 99B2964F A090C3A2"
                <> "233BA186 515BE7ED 1F612970 CEE2D7AF B81BDD76 2170481C D0069127"
                <> "D5B05AA9 93B4EA98 8D8FDDC1 86FFB7DC 90A6C08F 4DF435C9 34063199"
                <> "FFFFFFFF FFFFFFFF"

baseConfiguration :: SRPConf
baseConfiguration = { group: group1024
                    , k: (fromMaybe bigInt0 (toBigInt $ hex "7556AA045AEF2CDD07ABAF0F665C3E818913186F"))
                    , hash: hashFuncSHA256
                    , kdf: getPrepareX hashFuncSHA256
                    }

generateHashFunction :: HashingFunction -> HashFunction
generateHashFunction f = \list -> do
                            abs <- liftEffect $ concatArrayBuffers list
                            (digest f abs)

hashFuncSHA256 :: HashFunction
hashFuncSHA256 = generateHashFunction sha256

hashFuncSHA1 :: HashFunction
hashFuncSHA1 = generateHashFunction sha1

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
  abx <- srpConf.kdf salt p
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