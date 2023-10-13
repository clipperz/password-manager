module Functions.Communication.OneTimeShare where

import Affjax.RequestBody (json)
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (name, value)
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (find)
import Data.Either (Either)
import Data.Eq ((==))
import Data.Function (flip, (#), ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (fromArrayBuffer, hex, toArrayBuffer, toString)
import Data.HexString as Base
import Data.List (singleton)
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
import Effect.Aff.Class (liftAff)
import Effect.Exception as EX
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.EncodeDecode (cryptoKeyAES, decryptArrayBuffer, decryptJson, encryptArrayBuffer, encryptJson)
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
  key             <- liftAff $ randomArrayBuffer 32
  encryptedSecret <- liftAff $ (flip encryptJson) secret =<< cryptoKeyAES key
  
  let url  = joinWith "/" ["share"]
  let body = (json $ encodeJson 
    { secret:   fromArrayBuffer encryptedSecret
    , duration: unwrap $ fromDuration duration
    , version:  currentOneTimeSecretVersion 
    }
  )

  response <- manageGenericRequest url POST (Just body) RF.string
  if isStatusCodeOk response.status
    then liftAff $ do
      pinKey       <-  hashFuncSHA256 $ singleton (toArrayBuffer $ hex pin)
      encryptedKey <- (flip encryptArrayBuffer) key =<< cryptoKeyAES pinKey
      pure          $  Tuple (toString Base.Hex (fromArrayBuffer encryptedKey)) response.body
    else throwError $  ProtocolError $ ResponseError $ unwrap response.status

redeem :: forall a. DecodeJson a => String -> String -> String -> ExceptT AppError Aff a
redeem id cryptedKey pin = do
  let url = joinWith "/" ["redeem", id]

  response <- manageGenericRequest url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then do
      let version = fromMaybe "V1" (value <$> find (\header -> (name header) == oneTimeSecretVersionHeaderName) (response.headers))
      case version of
        "V1" -> do
          pinKey       <- liftAff $ (hashFuncSHA256 $ singleton (toArrayBuffer $ hex pin))
          decryptedKey <- ((flip decryptArrayBuffer) (toArrayBuffer $ hex cryptedKey) =<< cryptoKeyAES pinKey) # mapError "Get decrypted key"
          result       <- ((flip decryptJson)         response.body =<< cryptoKeyAES decryptedKey)             # mapError "Get decrypted blob"
          pure result 
        _    -> throwError $ ProtocolError $ IllegalResponse $ "version not found"
    else throwError $ ProtocolError $ ResponseError $ unwrap response.status
  
  where
    mapError :: forall a'. String -> Aff (Either EX.Error a') -> ExceptT AppError Aff a'
    mapError errorMessage either = withExceptT (\e -> ProtocolError $ CryptoError $ (errorMessage <> ": " <> show e)) (ExceptT either)
