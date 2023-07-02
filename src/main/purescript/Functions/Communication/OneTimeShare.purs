module Functions.Communication.OneTimeShare
  ( SecretData
  , SecretInfo
  , redeem
  , secretInfo
  , share
  )
  where

import Affjax.RequestBody (formData, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (decrypt, encrypt, raw, unwrapKey)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.DateTime (adjust)
import Data.Either (Either(..))
import Data.Formatter.DateTime (Formatter, FormatterCommand(..), format)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), fromArrayBuffer, hex, toArrayBuffer, toString)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Time.Duration (Days(..))
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRP (hashFuncSHA256)
import Debug (traceM)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.BlobFromArrayBuffer (blobFromArrayBuffer)
import Functions.EncodeDecode (decryptJson, encryptJson)
import Functions.Time (getCurrentDateTime)
import Web.XHR.FormData (EntryName(..), appendBlob, new)

type SecretData = { secret :: String
                  , password :: String
                  , duration :: Days
                  }

localizedFormatter :: Formatter --'12.13.52 3:30pm'
localizedFormatter = fromFoldable
  [ MonthTwoDigits
  , Placeholder "."
  , DayOfMonthTwoDigits
  , Placeholder "."
  , YearTwoDigits
  , Placeholder " "
  , Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  ]

share :: SecretData -> ExceptT AppError Aff String
share {secret, password, duration} = do
  key <- ExceptT $ Right <$> (hashFuncSHA256 ((toArrayBuffer $ hex password) : Nil))
  cryptoKey <- ExceptT $ Right <$> KI.importKey raw key (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  encryptedSecret <- ExceptT $ Right <$> encryptJson cryptoKey secret
  creationDate <- liftEffect getCurrentDateTime
  let expirationDate = fromMaybe creationDate (adjust duration creationDate)
  let url = joinWith "/" ["share"]
  let body = (json $ encodeJson { secret: fromArrayBuffer encryptedSecret, creationDate: (format localizedFormatter creationDate), expirationDate: (format localizedFormatter expirationDate) })
  -- body <- formData <$> (liftEffect $ do
  --   formData <- new
  --   appendBlob (EntryName "blob") (blobFromArrayBuffer encryptedSecret) Nothing formData
  --   pure $ formData
  -- )
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
    else do
      traceM $ response.statusText
      except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

redeem :: forall a. DecodeJson a => String -> String -> ExceptT AppError Aff a
redeem id password = do
  key <- ExceptT $ Right <$> (hashFuncSHA256 ((toArrayBuffer $ hex password) : Nil))
  cryptoKey <- ExceptT $ Right <$> KI.importKey raw key (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  let url = joinWith "/" ["redeem", id]
  response <- manageGenericRequest url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then do
      bytes <- except $ Right $ response.body
      liftEffect $ log ("BYTES" <> toString Dec (fromArrayBuffer bytes))
      withExceptT (\e -> ProtocolError $ CryptoError $ "Get decrypted blob: " <> show e) (ExceptT $ decryptJson cryptoKey bytes)
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status