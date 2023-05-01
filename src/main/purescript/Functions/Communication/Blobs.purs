module Functions.Communication.Blobs where

import Affjax.RequestBody (formData)
import Affjax.ResponseFormat as RF
import Affjax.Web as AXW
import Control.Bind (bind, discard, pure)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, fromArrayBuffer, toString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.BlobFromArrayBuffer (blobFromArrayBuffer)
import Functions.EncodeDecode (decryptJson)
import Web.XHR.FormData (EntryName(..), FileName(..), appendBlob, new)

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
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "blob") (blobFromArrayBuffer blob) (Just $ FileName (toString Hex (fromArrayBuffer hash))) formData
      pure $ formData
  )
  manageGenericRequest url POST (Just body) RF.string

deleteBlob :: HexString -> ExceptT AppError Aff String
deleteBlob reference = do
  let url = joinWith "/" ["blobs", toString Hex reference]
  encryptedCard <- getBlob reference
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "blob") (blobFromArrayBuffer encryptedCard) (Just $ FileName (toString Hex reference)) formData
      pure $ formData
  )
  response <- manageGenericRequest url DELETE (Just body) RF.string
  if isStatusCodeOk response.status
    then except $ Right $ response.body
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status
