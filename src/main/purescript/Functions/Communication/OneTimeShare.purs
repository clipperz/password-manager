module Functions.Communication.OneTimeShare where

import Affjax.RequestBody (json)
import Affjax.ResponseFormat as RF
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (decrypt, encrypt, raw, unwrapKey)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (fromArrayBuffer, hex, toArrayBuffer)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Time.Duration (Seconds, fromDuration)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRP (hashFuncSHA256)
import Effect.Aff (Aff)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.EncodeDecode (decryptJson, encryptJson)

type SecretData = { secret :: String
                  , password :: String
                  , duration :: Seconds
                  }

share :: SecretData -> ExceptT AppError Aff String
share {secret, password, duration} = do
  key <- ExceptT $ Right <$> (hashFuncSHA256 ((toArrayBuffer $ hex password) : Nil))
  cryptoKey <- ExceptT $ Right <$> KI.importKey raw key (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  encryptedSecret <- ExceptT $ Right <$> encryptJson cryptoKey secret 
  let url = joinWith "/" ["share"]
  let body = (json $ encodeJson { secret: fromArrayBuffer encryptedSecret, duration: unwrap $ fromDuration duration})

  response <- manageGenericRequest url POST (Just body) RF.string
  if isStatusCodeOk response.status
    then except $ Right $ response.body
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

type SecretInfo = { creationDate   :: String
                  , expirationDate :: String
                  }

secretInfo :: String -> ExceptT AppError Aff SecretInfo
secretInfo id = do
  let url = joinWith "/" ["oneTimeSecretInfo", id]
  response <- manageGenericRequest url GET Nothing RF.json
  if isStatusCodeOk response.status
    then withExceptT (\e -> ProtocolError (DecodeError (show e))) (except $ decodeJson response.body)
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

redeem :: forall a. DecodeJson a => String -> String -> ExceptT AppError Aff a
redeem id password = do
  key <- ExceptT $ Right <$> (hashFuncSHA256 ((toArrayBuffer $ hex password) : Nil))
  cryptoKey <- ExceptT $ Right <$> KI.importKey raw key (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  let url = joinWith "/" ["redeem", id]
  response <- manageGenericRequest url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then withExceptT (\e -> ProtocolError $ CryptoError $ "Get decrypted blob: " <> show e) (ExceptT $ decryptJson cryptoKey response.body)
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status