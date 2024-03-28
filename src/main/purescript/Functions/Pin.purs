module Functions.Pin where

import Control.Alt ((<#>))
import Control.Alternative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, throwError, withExceptT)
import Control.Semigroupoid ((<<<))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Either (note)
import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HexString (Base(..), HexString, fromArrayBuffer, hex, toArrayBuffer, toString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((*))
import Data.Show (show)
import Data.String.CodeUnits (length, splitAt)
import Data.Unit (Unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (InvalidStateError(..), AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.Pin (PasswordPin, passwordPinCodec)
import DataModel.SRPVersions.SRP (HashFunction)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.Communication.OneTimeShare (PIN)
import Functions.EncodeDecode (decryptJson, encryptJson, importCryptoKeyAesGCM)
import Functions.SRP (randomArrayBuffer)
import Web.Storage.Storage (Storage, removeItem, setItem)

makeKey :: String -> String
makeKey = (<>) "clipperz.is."

isPinValid :: PIN -> Boolean
isPinValid p = (length p) == 5

generateKeyFromPin :: HashFunction -> String -> Aff CryptoKey
generateKeyFromPin hashf pin = do
  pinBuffer <- hashf $ (toArrayBuffer $ hex pin) : Nil
  importCryptoKeyAesGCM pinBuffer

decryptPassphraseWithPin :: HashFunction -> PIN -> Maybe String -> Maybe HexString -> ExceptT AppError Aff Credentials
decryptPassphraseWithPin hashFunc pin username' pinEncryptedPassword' = do  
  username             <- except $ username'             # note (InvalidStateError (CorruptedSavedPassphrase "user not found in local storage"))
  pinEncryptedPassword <- except $ pinEncryptedPassword' # note (InvalidStateError (CorruptedSavedPassphrase "passphrase not found in local storage"))
  key <- liftAff $ generateKeyFromPin hashFunc pin
  { padding, passphrase } :: PasswordPin <- decryptJson passwordPinCodec key (toArrayBuffer pinEncryptedPassword) # ExceptT # withExceptT (ProtocolError <<< CryptoError <<< show)
  let split = toString Dec $ hex $ (splitAt ((length passphrase) - (padding * 2)) passphrase).before
  pure $ { username, password: split }

deleteCredentials :: Storage -> Effect Unit
deleteCredentials storage = do
  removeItem (makeKey "user")       storage
  removeItem (makeKey "passphrase") storage
  removeItem (makeKey "failures")   storage

saveCredentials :: AppState -> String -> Storage -> ExceptT AppError Aff HexString
saveCredentials {username: Just u, password: Just p, hash: hashf} pin storage = do
  key <- liftAff $ (generateKeyFromPin hashf pin)
  -- 256 bits
  let paddingBytesLength = (256 - 16 * length (toString Hex (hex p))) / 8
  paddingBytes     <- liftAff $ randomArrayBuffer paddingBytesLength
  paddedPassphrase <- liftAff $ fromArrayBuffer <$> (liftEffect $ concatArrayBuffers ((toArrayBuffer $ hex p) : paddingBytes : Nil))
  let obj = { padding: paddingBytesLength, passphrase: toString Hex paddedPassphrase }

  encryptedCredentials <- encryptJson passwordPinCodec key obj <#> fromArrayBuffer # liftAff
  liftEffect $ setItem (makeKey "user")        u                                  storage
  liftEffect $ setItem (makeKey "passphrase") (toString Hex encryptedCredentials) storage
  liftEffect $ setItem (makeKey "failures")   (show 0)                            storage

  pure encryptedCredentials
saveCredentials _ _ _ = throwError (InvalidStateError (MissingValue "Missing username from state"))
