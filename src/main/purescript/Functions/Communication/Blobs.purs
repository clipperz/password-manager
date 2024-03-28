module Functions.Communication.Blobs where

import Affjax.RequestBody (formData)
import Affjax.ResponseFormat as RF
import Affjax.Web as AXW
import Control.Bind (bind, discard, pure)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut (JsonCodec)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, toArrayBuffer, toString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Unit (Unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (ProxyResponse(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.Backend (ConnectionState, isStatusCodeOk, genericRequest)
import Functions.Communication.BlobFromArrayBuffer (blobFromArrayBuffer)
import Functions.EncodeDecode (decryptJson)
import Web.XHR.FormData (EntryName(..), FileName(..), appendBlob, new)

-- ----------------------------------------------------------------------------

getBlob :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse ArrayBuffer)
getBlob connectionState hash = do
  let url = joinWith "/" ["blobs", toString Hex hash]
  ProxyResponse proxy response <- genericRequest connectionState url GET Nothing RF.arrayBuffer
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)

getDecryptedBlob :: forall a. ConnectionState -> HexString -> JsonCodec a -> CryptoKey -> ExceptT AppError Aff (ProxyResponse a) 
getDecryptedBlob connectionState reference codec key = do
  ProxyResponse proxy blob <- getBlob connectionState reference
  decryptedBlob <- withExceptT (\e -> ProtocolError $ CryptoError $ "Get decrypted blob: " <> show e) (ExceptT $ decryptJson codec key blob)
  pure $ ProxyResponse proxy decryptedBlob

postBlob :: ConnectionState -> ArrayBuffer -> HexString -> HexString -> ExceptT AppError Aff (ProxyResponse (AXW.Response Unit))
postBlob connectionState blob blobReference blobIdentifier = do
  let url = joinWith "/" ["blobs"]
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "identifier") (blobFromArrayBuffer $ toArrayBuffer blobIdentifier) (Nothing)                                      formData
      appendBlob (EntryName "blob")       (blobFromArrayBuffer                 blob          ) (Just $ FileName (toString Hex blobReference)) formData
      pure $ formData
  )
  genericRequest connectionState url POST (Just body) RF.ignore

deleteBlob :: ConnectionState -> HexString -> HexString -> ExceptT AppError Aff (ProxyResponse Unit)
deleteBlob connectionState blobReference blobIdentifier = do
  let url = joinWith "/" ["blobs", toString Hex blobReference]
  body <- formData <$> (liftEffect $ do
      formData <- new
      appendBlob (EntryName "identifier") (blobFromArrayBuffer $ toArrayBuffer blobIdentifier) (Nothing) formData
      pure $ formData
  )
  ProxyResponse proxy response <- genericRequest connectionState url DELETE (Just body) RF.ignore
  if isStatusCodeOk response.status
    then pure       $ ProxyResponse proxy response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)
