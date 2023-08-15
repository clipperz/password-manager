module Functions.Communication.OneTimeShare where

import Affjax.RequestBody (json)
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (name, value)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (decrypt, encrypt, raw, unwrapKey)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (fromArrayBuffer, hex, toArrayBuffer, toString)
import Data.HexString as Base
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Time.Duration (Seconds, fromDuration)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRP (hashFuncSHA256)
import Effect.Aff (Aff)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.EncodeDecode (decryptArrayBuffer, decryptJson, encryptArrayBuffer, encryptJson)
import Functions.SRP (randomArrayBuffer)

currentOneTimeSecretVersion :: String
currentOneTimeSecretVersion = "V1"

oneTimeSecretVersionHeaderName :: String
oneTimeSecretVersionHeaderName = "clipperz-oneTimeSecret-version"

type SecretData = { secret   :: String
                  , pin      :: String
                  , duration :: Seconds
                  }

share :: SecretData -> ExceptT AppError Aff (Tuple String String)
share {secret, pin, duration} = do
  key <- ExceptT $ Right <$> randomArrayBuffer 32
  cryptoKey <- ExceptT $ Right <$> KI.importKey raw key (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  encryptedSecret <- ExceptT $ Right <$> encryptJson cryptoKey secret 
  
  let url = joinWith "/" ["share"]
  let body = (json $ encodeJson { secret: fromArrayBuffer encryptedSecret, duration: unwrap $ fromDuration duration, version: currentOneTimeSecretVersion })

  response <- manageGenericRequest url POST (Just body) RF.string
  if isStatusCodeOk response.status
    then do
      pinKey <- ExceptT $ Right <$> (hashFuncSHA256 ((toArrayBuffer $ hex pin) : Nil))
      cryptoPinKey <- ExceptT $ Right <$> KI.importKey raw pinKey (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      encryptedKey <- ExceptT $ Right <$> (encryptArrayBuffer cryptoPinKey key)
      except $ Right $ Tuple (toString Base.Hex (fromArrayBuffer encryptedKey)) response.body
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

redeem :: forall a. DecodeJson a => String -> String -> String -> ExceptT AppError Aff a
redeem id cryptedKey pin = do
  let url = joinWith "/" ["redeem", id]
  response <- manageGenericRequest url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then do
      let version = fromMaybe "V1" (value <$> find (\header -> (name header) == oneTimeSecretVersionHeaderName) (response.headers))
      case version of
        "V1" -> do
          pinKey       <- ExceptT $ Right <$> (hashFuncSHA256 ((toArrayBuffer $ hex pin) : Nil))
          cryptoPinKey <- ExceptT $ Right <$> KI.importKey raw pinKey (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
          decryptedKey <- withExceptT (\e -> ProtocolError $ CryptoError $ "Get decrypted key: " <> show e) (ExceptT $ decryptArrayBuffer cryptoPinKey (toArrayBuffer $ hex cryptedKey))
          cryptoKey <- ExceptT $ Right <$> KI.importKey raw decryptedKey (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
          withExceptT (\e -> ProtocolError $ CryptoError $ "Get decrypted blob: " <> show e) (ExceptT $ decryptJson cryptoKey response.body)
        _    ->  except $ Left $ ProtocolError $ IllegalResponse $ "version not found"
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status