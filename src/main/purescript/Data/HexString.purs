module Data.HexString
  ( Base(..)
  , HexString
  , fromArrayBuffer
  , fromBigInt
  , hex
  , isHex
  , splitHexInHalf
  , toArrayBuffer
  , toBigInt
  , toString
  )
  where

import Control.Semigroupoid ((>>>))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Array.NonEmpty (fromArray, toArray, singleton)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (rmap)
import Data.BigInt (BigInt, fromBase, toBase)
import Data.Either(hush)
import Data.Eq (class Eq, eq, (==))
import Data.EuclideanRing (mod, (/))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe(Maybe, maybe, fromMaybe)
import Data.Ord (class Ord)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String (toLower, toUpper)
import Data.String.CodeUnits (length, splitAt, fromCharArray)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (arrayOf1, elements)

foreign import hexEncode :: String -> String
foreign import hexDecode :: String -> String
foreign import hexToArrayBuffer :: String -> ArrayBuffer
foreign import arrayBufferToHex :: ArrayBuffer -> String

-- ----------------------------------------------------------------

data HexString = HexString String

derive instance ordHexString :: Ord HexString

instance eqHexString :: Eq HexString where
  eq (HexString h) (HexString h') =
    if eq h h' then true
    else if eq ("0" <> h) h' then true
    else if eq h ("0" <> h') then true
    else false

instance decodeJsonHexString :: DecodeJson HexString where
  decodeJson json = rmap (\s -> hex s) (decodeJson json)

instance encodeJsonHexString :: EncodeJson HexString where
  encodeJson hexString = encodeJson $ show hexString

instance showHexString :: Show HexString where
  show hexString = toString Hex hexString

instance arbitraryHexString :: Arbitrary HexString where
  arbitrary = (toArray >>> fromCharArray >>> HexString) <$> (arrayOf1 (elements hexChars))

hexChars = fromMaybe (singleton '0') $ fromArray ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'A', 'B', 'C', 'D', 'E', 'F']
-- ----------------------------------------------------------------

fromArrayBuffer :: ArrayBuffer -> HexString
fromArrayBuffer = arrayBufferToHex >>> hex

fromBigInt :: BigInt -> HexString
fromBigInt n = hex $ toBase 16 n

hex :: String -> HexString
hex s = normalizeHex $ HexString if isHex s then s else hexEncode s

isHex :: String -> Boolean
isHex s = maybe false (\reg -> test reg s) (hush (regex "^[0-9a-fA-F ]+$" noFlags))

splitHexInHalf :: HexString -> { before :: HexString, after :: HexString }
splitHexInHalf (HexString s) = let split = (splitAt ((length s) / 2) s)
  in { before: hex split.before, after: hex split.after }

normalizeHex :: HexString -> HexString
normalizeHex (HexString s) = let p = replaceAll (Pattern " ") (Replacement "") $ toUpper s
  in if mod (length p) 2 == 0 then HexString p else HexString $ "0" <> p 

toArrayBuffer :: HexString -> ArrayBuffer 
toArrayBuffer (HexString s) = hexToArrayBuffer s

toBigInt :: HexString -> Maybe BigInt
toBigInt (HexString s) = fromBase 16 s

data Base = Dec | Hex

toString :: Base -> HexString -> String
toString b (HexString s) = case b of
  Dec -> hexDecode s
  Hex -> toLower s
