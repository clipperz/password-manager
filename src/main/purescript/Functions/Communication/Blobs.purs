module Functions.Communication.Blobs where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import Functions.EncodeDecode (decryptJson)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)

-- ----------------------------------------------------------------------------

getBlob :: HexString -> ExceptT AppError Aff ArrayBuffer
getBlob hash = do
  let url = joinWith "/" ["blobs", show $ hash]
  response <- manageGenericRequest url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then except $ Right $ response.body
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status


getDecryptedBlob :: forall a. DecodeJson a => HexString -> CryptoKey -> ExceptT AppError Aff a 
getDecryptedBlob reference key = do
  blob <- getBlob reference
  withExceptT (\e -> ProtocolError $ CryptoError $ "Get decrypted blob: " <> show e) (ExceptT $ decryptJson key blob)

  
postBlob :: ArrayBuffer -> ArrayBuffer -> ExceptT AppError Aff (AXW.Response String)
postBlob blob hash = do
  let url = joinWith "/" ["blobs"]
  -- type BlobData = { data :: HexString, hash :: HexString }
  let body = json $ encodeJson $ { data: fromArrayBuffer blob, hash: fromArrayBuffer hash } :: RequestBody
  manageGenericRequest url POST (Just body) RF.string

deleteBlob :: HexString -> ExceptT AppError Aff String
deleteBlob  reference = do
  let url = joinWith "/" ["blobs", show reference]
  encryptedCard <- getBlob reference
  let body = json $ encodeJson $ { data: fromArrayBuffer encryptedCard, hash: reference } :: RequestBody
  response <- manageGenericRequest url DELETE (Just body) RF.string
  if isStatusCodeOk response.status
    then except $ Right $ response.body
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

-- postEncryptedBlob :: forall a. EncodeJson a => a -> CryptoKey -> ExceptT AppError Aff HexString
-- postEncryptedBlob blob key = do
--   encryptedBlob <- ExceptT $ Right <$> encryptJson key blob
--   response <- postBlob encryptedBlob
--   if isStatusCodeOk response.status
--     then except $ Right $ hex response.body
--     else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status
