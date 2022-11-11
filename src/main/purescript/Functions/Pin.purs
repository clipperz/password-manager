module Functions.Pin where

import Bytes (asArrayBuffer)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Control.Semigroupoid ((<<<))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.BigInt (fromInt)
import Data.Either (Either(..), note)
import Data.EuclideanRing ((/))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (hex, toArrayBuffer, toString, fromArrayBuffer, Base(..))
import Data.HeytingAlgebra ((&&))
import Data.List (List(..), (:))
import Data.Ord ((>), (<))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((*))
import Data.Show (show)
import Data.String.CodeUnits (splitAt, length)
import Data.Unit (Unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (HashFunction)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception as EX
import Effect.Fortuna (randomBytes)
import Functions.ArrayBuffer (concatArrayBuffers, bigIntToArrayBuffer)
import Functions.EncodeDecode (decryptJson, encryptJson)
import Functions.JSState (getAppState)
import Functions.State (getHashFunctionFromAppState)
import Web.Storage.Storage (getItem, setItem, removeItem, Storage)

makeKey :: String -> String
makeKey = (<>) "clipperz.is."

isPinValid :: Int -> Boolean
isPinValid p = p > 9999 && p < 100000

generateKeyFromPin :: HashFunction -> Int -> Aff CryptoKey
generateKeyFromPin hashf pin = do
  pinBuffer <- hashf $ (bigIntToArrayBuffer $ fromInt pin) : Nil
  KI.importKey raw pinBuffer (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]

decryptPassphrase :: Int -> String -> String -> ExceptT AppError Aff Credentials
decryptPassphrase pin user encryptedPassphrase = do
  state <- ExceptT $ liftEffect getAppState
  let hashf = getHashFunctionFromAppState state
  key <- ExceptT $ Right <$> (generateKeyFromPin hashf pin)
  let ab = toArrayBuffer $ hex $ encryptedPassphrase
  { padding: padding, passphrase: passphrase } :: { padding :: Int, passphrase :: String } <- withExceptT (\e -> InvalidStateError $ CorruptedSavedPassphrase $ "Decrypt passphrase: " <> EX.message e) $ ExceptT $ decryptJson key ab
  let split = toString Dec $ hex $ (splitAt ((length passphrase) - (padding * 2)) passphrase).before
  pure { username: user, password: split }

deleteCredentials :: Storage -> ExceptT AppError Effect Unit
deleteCredentials storage = liftEffect $ do
  removeItem (makeKey "user") storage
  removeItem (makeKey "passphrase") storage
  removeItem (makeKey "failures") storage

saveCredentials :: Int -> Storage -> ExceptT AppError Aff Unit
saveCredentials pin storage = do
  state@{ username, password } <- ExceptT $ liftEffect getAppState
  u <- except $ note (InvalidStateError (MissingValue "Missing username from state")) username
  p <- except $ note (InvalidStateError (MissingValue "Missing password from state")) password
  let hashf = getHashFunctionFromAppState state
  key <- ExceptT $ Right <$> (generateKeyFromPin hashf pin)

  -- 256 bits
  let paddingBytesLength = (256 - 16 * length (show (hex p))) / 8
  paddingBytes <- ExceptT $ (Right <<< asArrayBuffer) <$> (randomBytes paddingBytesLength)
  paddedPassphrase <- ExceptT $ (Right <<< fromArrayBuffer) <$> (liftEffect $ concatArrayBuffers ((toArrayBuffer $ hex p) : paddingBytes : Nil))
  let obj = { padding: paddingBytesLength, passphrase: paddedPassphrase }

  encryptedCredentials <- ExceptT $ Right <$> (encryptJson key obj)
  liftEffect $ setItem (makeKey "user") u storage -- save username  
  liftEffect $ setItem (makeKey "passphrase") (show $ fromArrayBuffer encryptedCredentials) storage -- save password
