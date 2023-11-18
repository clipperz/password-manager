module Functions.Pin where

import Bytes (asArrayBuffer)
import Control.Alternative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Control.Semigroupoid ((<<<))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Either (note)
import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HexString (Base(..), HexString, fromArrayBuffer, hex, toArrayBuffer, toString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((*))
import Data.Show (show)
import Data.String.CodeUnits (length, splitAt)
import Data.Unit (Unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (HashFunction)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Fortuna (randomBytes)
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.Communication.StatelessOneTimeShare (PIN)
import Functions.EncodeDecode (decryptJson, encryptJson)
import Functions.JSState (getAppState)
import Functions.State (getHashFunctionFromAppState)
import Web.Storage.Storage (Storage, removeItem, setItem)

makeKey :: String -> String
makeKey = (<>) "clipperz.is."

isPinValid :: Int -> Boolean
isPinValid p = (length $ show p) == 5

generateKeyFromPin :: HashFunction -> String -> Aff CryptoKey
generateKeyFromPin hashf pin = do
  pinBuffer <- hashf $ (toArrayBuffer $ hex pin) : Nil
  KI.importKey raw pinBuffer (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]

decryptPassphraseWithPin :: HashFunction -> PIN -> Maybe String -> Maybe HexString -> ExceptT AppError Aff Credentials
decryptPassphraseWithPin hashFunc pin username' pinEncryptedPassword' = do  
  username             <- except $ username'             # note (InvalidStateError (CorruptedSavedPassphrase "user not found in local storage"))
  pinEncryptedPassword <- except $ pinEncryptedPassword' # note (InvalidStateError (CorruptedSavedPassphrase "passphrase not found in local storage"))
  key <- liftAff $ generateKeyFromPin hashFunc pin
  { padding, passphrase } :: { padding :: Int, passphrase :: String } <- decryptJson key (toArrayBuffer pinEncryptedPassword) # ExceptT # withExceptT (ProtocolError <<< CryptoError <<< show)
  let split = toString Dec $ hex $ (splitAt ((length passphrase) - (padding * 2)) passphrase).before
  pure $ { username, password: split }

deleteCredentials :: Storage -> Effect Unit
deleteCredentials storage = do
  removeItem (makeKey "user")       storage
  removeItem (makeKey "passphrase") storage
  removeItem (makeKey "failures")   storage

saveCredentials :: Int -> Storage -> ExceptT AppError Aff Unit
saveCredentials pin storage = do
  state@{ username, password } <- ExceptT $ liftEffect getAppState
  u <- except $ username # note (InvalidStateError (MissingValue "Missing username from state")) 
  p <- except $ password # note (InvalidStateError (MissingValue "Missing password from state")) 
  let hashf = getHashFunctionFromAppState state
  key <- liftAff $ (generateKeyFromPin hashf $ show pin)

  -- 256 bits
  let paddingBytesLength = (256 - 16 * length (toString Hex (hex p))) / 8
  paddingBytes     <- liftAff $ asArrayBuffer   <$> (randomBytes paddingBytesLength)
  paddedPassphrase <- liftAff $ fromArrayBuffer <$> (liftEffect $ concatArrayBuffers ((toArrayBuffer $ hex p) : paddingBytes : Nil))
  let obj = { padding: paddingBytesLength, passphrase: paddedPassphrase }

  encryptedCredentials <- liftAff $ encryptJson key obj
  liftEffect $ setItem (makeKey "user")        u                                                    storage
  liftEffect $ setItem (makeKey "passphrase") (toString Hex $ fromArrayBuffer encryptedCredentials) storage
  liftEffect $ setItem (makeKey "failures")   (show 0)                                              storage
