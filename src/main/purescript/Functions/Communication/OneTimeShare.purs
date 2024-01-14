module Functions.Communication.OneTimeShare where

import Affjax.RequestBody (json)
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (name, value)
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Data.Array (find)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut (JsonCodec, encode)
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
import DataModel.AppError (AppError(..))
import DataModel.AppState (ProxyResponse(..))
import DataModel.Communication.OneTimeShare (SecretVersion(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.OneTimeShareCodec as OneTimeShareCodec
import DataModel.SRP (hashFuncSHA256)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception as EX
import Functions.Communication.Backend (ConnectionState, isStatusCodeOk, shareRequest)
import Functions.EncodeDecode (cryptoKeyAES, decryptArrayBuffer, decryptJson, encryptArrayBuffer, encryptJson)
import Functions.SRP (randomArrayBuffer)

secretVersionFromString :: String -> ExceptT AppError Aff SecretVersion
secretVersionFromString "V_1" = pure V_1
secretVersionFromString s     = throwError $ InvalidVersioning "SecretVersion" s

oneTimeSecretVersionHeaderName :: String
oneTimeSecretVersionHeaderName = "clipperz-onetimesecret-version"

type UUID = String

share :: ConnectionState -> ArrayBuffer -> Seconds -> ExceptT AppError Aff UUID
share connectionState encryptedSecret duration = do
  let url  = joinWith "/" ["share"]
  let body = (json $ encode OneTimeShareCodec.secretRequestDataCodec 
      { secret:   fromArrayBuffer encryptedSecret
      , duration: fromDuration duration
      , version:  V_1
      }
    )

  ProxyResponse _ response <- shareRequest connectionState url POST (Just body) RF.string
  if isStatusCodeOk response.status
    then pure $ response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)

redeem :: ConnectionState -> UUID -> ExceptT AppError Aff (Tuple SecretVersion ArrayBuffer)
redeem connectionState id = do
  let url = joinWith "/" ["redeem", id]

  ProxyResponse _ response <- shareRequest connectionState url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then do
      version <- secretVersionFromString $ fromMaybe "V_1" (value <$> find (\header -> (name header) == oneTimeSecretVersionHeaderName) (response.headers))
      pure $ Tuple version response.body
    else throwError $ ProtocolError $ ResponseError $ unwrap response.status

-- ====================================================================================

type PIN = String
type EncryptedContent = ArrayBuffer
type EncryptionKey = ArrayBuffer

encryptSecret :: forall a. JsonCodec a -> a -> Aff (Tuple EncryptionKey EncryptedContent)
encryptSecret codec secret =  do
  key             <- liftAff $ randomArrayBuffer 32
  encryptedSecret <- liftAff $ (\cryptoKey -> encryptJson codec cryptoKey secret) =<< cryptoKeyAES key
  pure $ Tuple key encryptedSecret

encryptKeyWithPin :: EncryptionKey -> PIN -> Aff EncryptedContent
encryptKeyWithPin key pin = do
  pinKey       <-  hashFuncSHA256 $ singleton (toArrayBuffer $ hex pin)
  encryptedKey <- (flip encryptArrayBuffer) key =<< cryptoKeyAES pinKey
  pure encryptedKey

decryptSecret :: forall a. JsonCodec a -> SecretVersion -> PIN -> EncryptedContent -> EncryptedContent -> ExceptT AppError Aff a
decryptSecret codec V_1 pin encryptedKey encryptedSecret = do
  pinKey       <- liftAff $ (hashFuncSHA256 $ singleton (toArrayBuffer $ hex pin))
  decryptedKey <- ((flip decryptArrayBuffer) (toArrayBuffer $ fromArrayBuffer encryptedKey) =<< cryptoKeyAES pinKey)       # mapError "Get decrypted key"
  result       <- ((\cryptoKey -> decryptJson codec cryptoKey encryptedSecret)              =<< cryptoKeyAES decryptedKey) # mapError "Get decrypted blob"
  pure result

  where
    mapError :: forall a'. String -> Aff (Either EX.Error a') -> ExceptT AppError Aff a'
    mapError errorMessage either = withExceptT (\e -> ProtocolError $ CryptoError $ (errorMessage <> ": " <> show e)) (ExceptT either)

