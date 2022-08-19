module Data.HexString
  ( Base(..)
  , HexString
  , fromArrayBuffer
  , fromBigInt
  , hex
  , isHex
  , toArrayBuffer
  , toBigInt
  , toString
  )
  where

import Control.Semigroupoid ((>>>))
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (rmap)
import Data.BigInt (BigInt, fromBase, toBase)
import Data.Either(hush)
import Data.Eq (class Eq, (==))
import Data.EuclideanRing (mod)
import Data.Function (($))
import Data.Maybe(Maybe, maybe)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String (toLower, toUpper)
import Data.String.CodeUnits (length)
import Data.String.Common (replaceAll)
import Data.String.Pattern (Pattern(..), Replacement(..))
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (noFlags)

foreign import hexEncode :: String -> String
foreign import hexDecode :: String -> String
foreign import hexToArrayBuffer :: String -> ArrayBuffer
foreign import arrayBufferToHex :: ArrayBuffer -> String

-- ----------------------------------------------------------------

data HexString = HexString String

derive instance eqHexString :: Eq HexString

instance decodeJsonHexString :: DecodeJson HexString where
  decodeJson json = rmap (\s -> hex s) (decodeJson json)

instance encodeJsonHexString :: EncodeJson HexString where
  encodeJson hexString = encodeJson $ show hexString

instance showHexString :: Show HexString where
  show hexString = toString Hex hexString

-- ----------------------------------------------------------------

fromArrayBuffer :: ArrayBuffer -> HexString
fromArrayBuffer = arrayBufferToHex >>> hex

fromBigInt :: BigInt -> HexString
fromBigInt n = hex $ toBase 16 n

hex :: String -> HexString
hex s = normalizeHex $ HexString if isHex s then s else hexEncode s

isHex :: String -> Boolean
isHex s = maybe false (\reg -> test reg s) (hush (regex "^[0-9a-fA-F ]+$" noFlags))

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
