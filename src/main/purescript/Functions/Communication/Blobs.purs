module Functions.Communication.Blobs where

import Affjax.RequestBody (formData)
import Affjax.ResponseFormat as RF
import Affjax.Web as AXW
import Control.Bind (bind, discard, pure)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, fromArrayBuffer, toString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import DataModel.AppError (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.AppState (ProxyResponse(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.Backend (ConnectionState, isStatusCodeOk, manageGenericRequest)
import Functions.Communication.BlobFromArrayBuffer (blobFromArrayBuffer)
import Functions.EncodeDecode (decryptJson)
import Web.XHR.FormData (EntryName(..), FileName(..), appendBlob, new)

-- ----------------------------------------------------------------------------

getBlob :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse ArrayBuffer)
getBlob connectionState hash = do
  let url = joinWith "/" ["blobs", show $ hash]
  ProxyResponse proxy response <- manageGenericRequest connectionState url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)

getDecryptedBlob :: forall a. DecodeJson a => ConnectionState -> HexString -> CryptoKey -> ExceptT AppError Aff (ProxyResponse a) 
getDecryptedBlob connectionState reference key = do
  ProxyResponse proxy blob <- getBlob connectionState reference
  decryptedBlob <- withExceptT (\e -> ProtocolError $ CryptoError $ "Get decrypted blob: " <> show e) (ExceptT $ decryptJson key blob)
  pure $ ProxyResponse proxy decryptedBlob

postBlob :: ConnectionState -> ArrayBuffer -> ArrayBuffer -> ExceptT AppError Aff (ProxyResponse (AXW.Response String))
postBlob connectionState blob hash = do
  let url = joinWith "/" ["blobs"]
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "blob") (blobFromArrayBuffer blob) (Just $ FileName (toString Hex (fromArrayBuffer hash))) formData
      pure $ formData
  )
  manageGenericRequest connectionState url POST (Just body) RF.string

deleteBlob :: ConnectionState -> ArrayBuffer -> HexString -> ExceptT AppError Aff (ProxyResponse String)
deleteBlob connectionState encryptedBlob reference = do
  let url = joinWith "/" ["blobs"]
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "blob") (blobFromArrayBuffer encryptedBlob) (Just $ FileName (toString Hex reference)) formData
      pure $ formData
  )
  ProxyResponse proxy response <- manageGenericRequest connectionState url DELETE (Just body) RF.string
  if isStatusCodeOk response.status
    then pure       $ ProxyResponse proxy response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)

deleteBlobWithReference :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse String)
deleteBlobWithReference connectionState reference = do
  ProxyResponse proxy' encryptedBlob <- getBlob connectionState reference
  deleteBlob connectionState {proxy = proxy'} encryptedBlob reference

