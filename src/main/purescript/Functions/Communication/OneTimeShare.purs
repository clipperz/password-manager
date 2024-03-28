module Functions.Communication.OneTimeShare where

import Affjax.RequestBody (json)
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (name, value)
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Category ((>>>))
import Control.Monad.Except (except)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Data.Argonaut.Parser as P
import Data.Array (find)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut (JsonCodec, decode, encode)
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
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.OneTimeShare (SecretVersion(..), secretRequestDataCodec, secretVersionCodec)
import DataModel.SRPVersions.SRP (hashFuncSHA256)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception as EX
import Functions.Communication.Backend (ConnectionState, isStatusCodeOk, shareRequest)
import Functions.EncodeDecode (importCryptoKeyAesGCM, decryptArrayBuffer, decryptJson, encryptArrayBuffer, encryptJson)
import Functions.SRP (randomArrayBuffer)

oneTimeSecretVersionHeaderName :: String
oneTimeSecretVersionHeaderName = "clipperz-onetimesecret-version"

type UUID = String

share :: ConnectionState -> ArrayBuffer -> Seconds -> ExceptT AppError Aff UUID
share connectionState encryptedSecret duration = do
  let url  = joinWith "/" ["share"]
  let body = (json $ encode secretRequestDataCodec 
      { secret:   fromArrayBuffer encryptedSecret
      , duration: fromDuration duration
      , version:  SecretVersion_1
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
      secretVersion :: SecretVersion <- ((value <$> find (\header -> (name header) == oneTimeSecretVersionHeaderName) (response.headers)) <#> (\headerValue -> do
        headerJson <- (except $ P.jsonParser headerValue)             # withExceptT (show >>> DecodeError >>> ProtocolError)
        (              except $ decode secretVersionCodec headerJson) # withExceptT (show >>> DecodeError >>> ProtocolError)
      )) # fromMaybe (pure SecretVersion_1)

      pure $ Tuple secretVersion response.body
    else throwError $ ProtocolError $ ResponseError $ unwrap response.status

-- ====================================================================================

type PIN = String
type EncryptedContent = ArrayBuffer
type EncryptionKey = ArrayBuffer

encryptSecret :: forall a. JsonCodec a -> a -> Aff (Tuple EncryptionKey EncryptedContent)
encryptSecret codec secret =  do
  key             <- liftAff $ randomArrayBuffer 32
  encryptedSecret <- liftAff $ (\cryptoKey -> encryptJson codec cryptoKey secret) =<< importCryptoKeyAesGCM key
  pure $ Tuple key encryptedSecret

encryptKeyWithPin :: EncryptionKey -> PIN -> Aff EncryptedContent
encryptKeyWithPin key pin = do
  pinKey       <-  hashFuncSHA256 $ singleton (toArrayBuffer $ hex pin)
  encryptedKey <- (flip encryptArrayBuffer) key =<< importCryptoKeyAesGCM pinKey
  pure encryptedKey

decryptSecret :: forall a. JsonCodec a -> SecretVersion -> PIN -> EncryptedContent -> EncryptedContent -> ExceptT AppError Aff a
decryptSecret codec version pin encryptedKey encryptedSecret = case version of
  SecretVersion_1 -> do
    pinKey       <- liftAff $ (hashFuncSHA256 $ singleton (toArrayBuffer $ hex pin))
    decryptedKey <- ((flip decryptArrayBuffer) (toArrayBuffer $ fromArrayBuffer encryptedKey) =<< importCryptoKeyAesGCM pinKey      ) # mapError "Get decrypted key"
    result       <- ((\cryptoKey -> decryptJson codec cryptoKey encryptedSecret)              =<< importCryptoKeyAesGCM decryptedKey) # mapError "Get decrypted blob"
    pure result

  where
    mapError :: forall a'. String -> Aff (Either EX.Error a') -> ExceptT AppError Aff a'
    mapError errorMessage either = withExceptT (\e -> ProtocolError $ CryptoError $ (errorMessage <> ": " <> show e)) (ExceptT either)