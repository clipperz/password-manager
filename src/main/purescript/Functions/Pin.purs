module Functions.Pin where

import Bytes (asArrayBuffer)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, throwError)
import Control.Semigroupoid ((<<<))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Either (Either(..), note, hush)
import Data.Eq ((==))
import Data.EuclideanRing ((/))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HexString (Base(..), fromArrayBuffer, hex, toArrayBuffer, toString)
import Data.Int (fromString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((*), (+))
import Data.Show (show)
import Data.String.CodeUnits (length, splitAt)
import Data.Unit (Unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (HashFunction)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Fortuna (randomBytes)
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.EncodeDecode (decryptJson, encryptJson)
import Functions.JSState (getAppState)
import Functions.State (getHashFunctionFromAppState)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem, removeItem, Storage)

makeKey :: String -> String
makeKey = (<>) "clipperz.is."

isPinValid :: Int -> Boolean
isPinValid p = (length $ show p) == 5

generateKeyFromPin :: HashFunction -> String -> Aff CryptoKey
generateKeyFromPin hashf pin = do
  pinBuffer <- hashf $ (toArrayBuffer $ hex pin) : Nil
  KI.importKey raw pinBuffer (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]

decryptPassphraseWithRemoval :: HashFunction -> String -> String -> String -> ExceptT AppError Aff Credentials
decryptPassphraseWithRemoval hashFunc pin user encryptedPassphrase = do
  key <- liftAff $ generateKeyFromPin hashFunc pin
  let ab = toArrayBuffer $ hex $ encryptedPassphrase
  eitherData :: Maybe { padding :: Int, passphrase :: String } <- liftAff $ hush <$> decryptJson key ab
  storage <- liftAff $ liftEffect $ window >>= localStorage
  case eitherData of
    Just { padding: padding, passphrase: passphrase } -> do
      let split = toString Dec $ hex $ (splitAt ((length passphrase) - (padding * 2)) passphrase).before
      liftAff $ liftEffect $ setItem (makeKey "failures") (show 0) storage
      except $ Right { username: user, password: split }
    Nothing -> do
      failures <- liftAff $ liftEffect $ getItem (makeKey "failures") storage
      let count = (((fromMaybe 0) <<< fromString <<< (fromMaybe "")) failures) + 1
      if count < 3 then do
        liftAff $ liftEffect $ setItem (makeKey "failures") (show count) storage
        throwError $ InvalidStateError (CorruptedSavedPassphrase "Saved passphrase could not be decrypted")
      else do
        mapExceptT liftEffect $ deleteCredentials storage
        throwError $ InvalidStateError (CorruptedSavedPassphrase "Saved passphrase could not be decrypted, removing saved data")

deleteCredentials :: Storage -> ExceptT AppError Effect Unit
deleteCredentials storage = do
  liftEffect $ removeItem (makeKey "user")       storage
  liftEffect $ removeItem (makeKey "passphrase") storage
  liftEffect $ removeItem (makeKey "failures")   storage

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
  liftEffect $ setItem (makeKey "user")        u                                                    storage -- save username  
  liftEffect $ setItem (makeKey "passphrase") (toString Hex $ fromArrayBuffer encryptedCredentials) storage -- save password
