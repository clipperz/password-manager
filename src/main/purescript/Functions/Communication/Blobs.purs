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
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.StatelessAppState (ProxyResponse(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.BlobFromArrayBuffer (blobFromArrayBuffer)
import Functions.Communication.StatelessBackend (ConnectionState)
import Functions.Communication.StatelessBackend as Stateless
import Functions.EncodeDecode (decryptJson)
import Web.XHR.FormData (EntryName(..), FileName(..), appendBlob, new)

-- ----------------------------------------------------------------------------

-- TODO REMOVE
getBlob :: HexString -> ExceptT AppError Aff ArrayBuffer
getBlob hash = do
  let url = joinWith "/" ["blobs", show $ hash]
  response <- manageGenericRequest url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then pure response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)
-- ------------

getStatelessBlob :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse ArrayBuffer)
getStatelessBlob connectionState hash = do
  let url = joinWith "/" ["blobs", show $ hash]
  ProxyResponse proxy response <- Stateless.manageGenericRequest connectionState url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)

-- TODO REMOVE
getDecryptedBlob :: forall a. DecodeJson a => HexString -> CryptoKey -> ExceptT AppError Aff a 
getDecryptedBlob reference key = do
  blob <- getBlob reference
  withExceptT (\e -> ProtocolError $ CryptoError $ "Get decrypted blob: " <> show e) (ExceptT $ decryptJson key blob)
-- ------------

getStatelessDecryptedBlob :: forall a. DecodeJson a => ConnectionState -> HexString -> CryptoKey -> ExceptT AppError Aff (ProxyResponse a) 
getStatelessDecryptedBlob connectionState reference key = do
  ProxyResponse proxy blob <- getStatelessBlob connectionState reference
  decryptedBlob <- withExceptT (\e -> ProtocolError $ CryptoError $ "Get decrypted blob: " <> show e) (ExceptT $ decryptJson key blob)
  pure $ ProxyResponse proxy decryptedBlob

-- TODO REMOVE
postBlob :: ArrayBuffer -> ArrayBuffer -> ExceptT AppError Aff (AXW.Response String)
postBlob blob hash = do
  let url = joinWith "/" ["blobs"]
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "blob") (blobFromArrayBuffer blob) (Just $ FileName (toString Hex (fromArrayBuffer hash))) formData
      pure $ formData
  )
  manageGenericRequest url POST (Just body) RF.string
-- ------------

postStatelessBlob :: ConnectionState -> ArrayBuffer -> ArrayBuffer -> ExceptT AppError Aff (ProxyResponse (AXW.Response String))
postStatelessBlob connectionState blob hash = do
  let url = joinWith "/" ["blobs"]
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "blob") (blobFromArrayBuffer blob) (Just $ FileName (toString Hex (fromArrayBuffer hash))) formData
      pure $ formData
  )
  Stateless.manageGenericRequest connectionState url POST (Just body) RF.string

-- TODO REMOVE
deleteBlob :: HexString -> ExceptT AppError Aff String
deleteBlob reference = do
  let url = joinWith "/" ["blobs"]
  encryptedCard <- getBlob reference
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "blob") (blobFromArrayBuffer encryptedCard) (Just $ FileName (toString Hex reference)) formData
      pure $ formData
  )
  response <- manageGenericRequest url DELETE (Just body) RF.string
  if isStatusCodeOk response.status
    then pure response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)
-- -------------

deleteStatelessBlob :: ConnectionState -> ArrayBuffer -> HexString -> ExceptT AppError Aff (ProxyResponse String)
deleteStatelessBlob connectionState encryptedBlob reference = do
  let url = joinWith "/" ["blobs"]
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "blob") (blobFromArrayBuffer encryptedBlob) (Just $ FileName (toString Hex reference)) formData
      pure $ formData
  )
  ProxyResponse proxy response <- Stateless.manageGenericRequest connectionState url DELETE (Just body) RF.string
  if isStatusCodeOk response.status
    then pure       $ ProxyResponse proxy response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)

deleteBlobWithReference :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse String)
deleteBlobWithReference connectionState reference = do
  ProxyResponse proxy' encryptedBlob <- getStatelessBlob connectionState reference
  deleteStatelessBlob connectionState {proxy = proxy'} encryptedBlob reference

