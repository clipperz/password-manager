module Functions.Communication.StatelessOneTimeShare where

import Affjax.RequestBody (json)
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (name, value)
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Array (find)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either)
import Data.Eq ((==))
import Data.Function (flip, (#), ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (fromArrayBuffer, hex, toArrayBuffer)
import Data.List (singleton)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Time.Duration (Seconds, fromDuration)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (Tuple3, tuple3)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRP (hashFuncSHA256)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception as EX
import Functions.Communication.StatelessBackend (Proxy, ConnectionState, isStatusCodeOk, manageGenericRequest)
import Functions.EncodeDecode (cryptoKeyAES, decryptArrayBuffer, decryptJson, encryptArrayBuffer, encryptJson)
import Functions.SRP (randomArrayBuffer)

-- currentOneTimeSecretVersion :: String
-- currentOneTimeSecretVersion = "V1"

data SecretVersion = V_1

secretVersionFromString :: String -> ExceptT AppError Aff SecretVersion
secretVersionFromString "V_1" = pure V_1
secretVersionFromString s     = throwError $ InvalidVersioning "SecretVersion" s

oneTimeSecretVersionHeaderName :: String
oneTimeSecretVersionHeaderName = "clipperz-oneTimeSecret-version"

type SecretData = { secret   :: String
                  , pin      :: String
                  , duration :: Seconds
                  }

type UUID = String


instance secretVersionDeriveJson :: EncodeJson SecretVersion where
  encodeJson V_1 = encodeJson "V_1"

share :: ConnectionState -> ArrayBuffer -> Seconds -> ExceptT AppError Aff UUID
share connectionState encryptedSecret duration = do
  let url  = joinWith "/" ["share"]
  let body = (json $ encodeJson 
      { secret:   fromArrayBuffer encryptedSecret
      , duration: unwrap $ fromDuration duration
      , version:  V_1
      }
    )

  Tuple _ response <- manageGenericRequest connectionState url POST (Just body) RF.string
  if isStatusCodeOk response.status
    then pure $ response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)

redeem :: ConnectionState -> UUID -> ExceptT AppError Aff (Tuple3 Proxy SecretVersion ArrayBuffer)
redeem connectionState id = do
  let url = joinWith "/" ["redeem", id]

  Tuple proxy response <- manageGenericRequest connectionState url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then do
      version <- secretVersionFromString $ fromMaybe "V1" (value <$> find (\header -> (name header) == oneTimeSecretVersionHeaderName) (response.headers))
      pure $ tuple3 proxy version response.body
    else throwError $ ProtocolError $ ResponseError $ unwrap response.status

-- ====================================================================================

type EncryptedContent = ArrayBuffer
type PIN = String
type EncryptionKey = ArrayBuffer
encryptSecret :: forall a. EncodeJson a => a -> Aff (Tuple EncryptionKey EncryptedContent)
encryptSecret secret =  do
  key             <- liftAff $ randomArrayBuffer 32
  encryptedSecret <- liftAff $ (flip encryptJson) secret =<< cryptoKeyAES key
  pure $ Tuple key encryptedSecret

encryptKeyWithPin :: EncryptionKey -> PIN -> Aff EncryptedContent
encryptKeyWithPin key pin = do
  pinKey       <-  hashFuncSHA256 $ singleton (toArrayBuffer $ hex pin)
  encryptedKey <- (flip encryptArrayBuffer) key =<< cryptoKeyAES pinKey
  pure encryptedKey

decryptSecret :: forall a. DecodeJson a => SecretVersion -> PIN -> EncryptedContent -> EncryptedContent -> ExceptT AppError Aff a
decryptSecret V_1 pin encryptedKey encryptedSecret = do
  pinKey       <- liftAff $ (hashFuncSHA256 $ singleton (toArrayBuffer $ hex pin))
  decryptedKey <- ((flip decryptArrayBuffer) (toArrayBuffer $ fromArrayBuffer encryptedKey) =<< cryptoKeyAES pinKey) # mapError "Get decrypted key"
  result       <- ((flip decryptJson)         encryptedSecret =<< cryptoKeyAES decryptedKey)                         # mapError "Get decrypted blob"
  pure result

  where
    mapError :: forall a'. String -> Aff (Either EX.Error a') -> ExceptT AppError Aff a'
    mapError errorMessage either = withExceptT (\e -> ProtocolError $ CryptoError $ (errorMessage <> ": " <> show e)) (ExceptT either)

