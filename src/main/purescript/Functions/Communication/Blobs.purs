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
import Data.Show (show)
import Data.String.Common (joinWith)
import DataModel.AppState (AppState, AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import Functions.EncodeDecode (decryptJson)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest, manageGenericRequest)

-- ----------------------------------------------------------------------------

getBlob :: HexString -> ExceptT AppError Aff (AXW.Response ArrayBuffer)
getBlob hash = do
  let url = joinWith "/" ["blobs", show $ hash]
  manageGenericRequest url GET Nothing RF.arrayBuffer

getDecryptedBlob :: forall a. DecodeJson a => HexString -> CryptoKey -> ExceptT AppError Aff a 
getDecryptedBlob reference key = do
  response <- getBlob reference
  if isStatusCodeOk response.status
    then withExceptT (\e -> ProtocolError $ CryptoError $ show e) (ExceptT $ decryptJson key response.body)
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

postBlob :: ArrayBuffer -> ExceptT AppError Aff (AXW.Response String)
postBlob blob = do
  let url = joinWith "/" ["blobs"]
  let body = json $ encodeJson (fromArrayBuffer blob) :: RequestBody
  manageGenericRequest url POST (Just body) RF.string

