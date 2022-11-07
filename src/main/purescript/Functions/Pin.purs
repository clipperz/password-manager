module Functions.Pin where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT)
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.BigInt (fromInt)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (hex, toArrayBuffer, toString, Base(..))
import Data.HeytingAlgebra ((&&), not)
import Data.List (List(..), (:))
import Data.Ord ((>), (<))
import Data.Ring ((-))
import Data.Semigroup ((<>))
import Data.Semiring ((*))
import Data.String.CodeUnits (splitAt, length)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (HashFunction)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception as EX
import Functions.ArrayBuffer (bigIntToArrayBuffer)
import Functions.EncodeDecode (decryptJson)
import Functions.JSState (getAppState)
import Functions.State (getHashFunctionFromAppState)


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
  state@{ username, password } <- ExceptT $ liftEffect getAppState
  let hashf = getHashFunctionFromAppState state
  key <- ExceptT $ Right <$> (liftAff $ generateKeyFromPin hashf pin)
  let ab = toArrayBuffer $ hex $ encryptedPassphrase
  { padding: padding, passphrase: passphrase } :: { padding :: Int, passphrase :: String } <- withExceptT (\e -> InvalidStateError $ CorruptedSavedPassphrase $ "Decrypt passphrase: " <> EX.message e) $ ExceptT $ decryptJson key ab
  let split = toString Dec $ hex $ (splitAt ((length passphrase) - (padding * 2)) passphrase).before
  pure { username: user, password: split }
