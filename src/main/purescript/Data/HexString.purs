module Data.HexString
  ( Base(..)
  , HexString(..)
  , fromArrayBuffer
  , fromBigInt
  , hex
  , hexChars
  , hexLength
  , hexStringCodec
  , isHex
  , isHexRegex
  , splitHexAt
  , splitHexInHalf
  , toArrayBuffer
  , toBigInt
  , toString
  )
  where

import Control.Semigroupoid ((>>>))
import Data.Array.NonEmpty (fromArray, toArray, singleton, NonEmptyArray)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.BigInt (BigInt, fromBase, toBase)
import Data.Codec.Argonaut as CA
import Data.Either (hush, Either)
import Data.Eq (class Eq, eq, (==))
import Data.EuclideanRing (mod, (/))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe, maybe, fromMaybe)
import Data.Newtype (class Newtype)
import Data.Ord (class Ord)
import Data.Profunctor (wrapIso)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.String (toLower, toUpper)
import Data.String.CodeUnits (dropWhile, fromCharArray, length, splitAt)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (regex, Regex, test)
import Data.String.Regex.Flags (noFlags)
import Test.QuickCheck.Arbitrary (class Arbitrary)
import Test.QuickCheck.Gen (arrayOf1, elements)

foreign import hexEncode :: String -> String
foreign import hexDecode :: String -> String
foreign import hexToArrayBuffer :: String -> ArrayBuffer
foreign import arrayBufferToHex :: ArrayBuffer -> String

-- ----------------------------------------------------------------

newtype HexString = HexString String

derive instance ordHexString :: Ord HexString

derive instance newtypeHexString :: Newtype HexString _

instance eqHexString :: Eq HexString where
  eq (HexString h) (HexString h') = eq (dropWhile (_ == '0') h) (dropWhile (_ == '0') h')

instance showHexString :: Show HexString where
  show hexString = toString Hex hexString

instance arbitraryHexString :: Arbitrary HexString where
  arbitrary = (toArray >>> fromCharArray >>> hex) <$> (arrayOf1 (elements hexChars))

hexChars :: NonEmptyArray Char
hexChars = fromMaybe (singleton '0') $ fromArray ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', 'A', 'B', 'C', 'D', 'E', 'F', 'a', 'b', 'c', 'd', 'e', 'f']
-- ----------------------------------------------------------------

hexStringCodec :: CA.JsonCodec HexString
hexStringCodec = wrapIso HexString CA.string

fromArrayBuffer :: ArrayBuffer -> HexString
fromArrayBuffer = arrayBufferToHex >>> hex

fromBigInt :: BigInt -> HexString
fromBigInt n = hex $ toBase 16 n

hex :: String -> HexString
hex s = normalizeHex $ HexString if isHex s then s else hexEncode s

hexLength :: HexString -> Int
hexLength (HexString s) = length s

isHexRegex :: Either String Regex
isHexRegex = (regex "^[0-9a-fA-F ]+$" noFlags)

isHex :: String -> Boolean
isHex s = maybe false (\reg -> test reg s) (hush isHexRegex)

splitHexInHalf :: HexString -> { before :: HexString, after :: HexString }
splitHexInHalf hexString = splitHexAt (hexLength hexString / 2) hexString

splitHexAt :: Int -> HexString -> { before :: HexString, after :: HexString }
splitHexAt position (HexString s) = let split = (splitAt position s)
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
