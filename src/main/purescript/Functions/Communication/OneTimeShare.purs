module Functions.Communication.OneTimeShare where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (decrypt, encrypt, raw, unwrapKey)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), fromArrayBuffer, hex, toArrayBuffer, toString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRP (hashFuncSHA256)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.EncodeDecode (decryptJson, encryptJson)

share :: String -> String -> ExceptT AppError Aff String
share secret password = do
  key <- ExceptT $ Right <$> (hashFuncSHA256 ((toArrayBuffer $ hex password) : Nil))
  cryptoKey <- ExceptT $ Right <$> KI.importKey raw key (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  encryptedSecret <- ExceptT $ Right <$> encryptJson cryptoKey secret
  let url = joinWith "/" ["share"]
  -- let body = json $ encodeJson $ { data: fromArrayBuffer encryptedSecret } :: RequestBody
  let body = json $ encodeJson $ fromArrayBuffer encryptedSecret :: RequestBody
  response <- manageGenericRequest url POST (Just body) RF.string
  if isStatusCodeOk response.status
    then except $ Right $ response.body
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

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