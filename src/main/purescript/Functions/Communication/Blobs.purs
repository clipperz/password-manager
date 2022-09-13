module Functions.Communication.Blobs where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, except, withExceptT)
import Control.Monad.State (StateT, mapStateT)
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
import DataModel.AppState (AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import EncodeDecode (decryptJson)
import Functions.Communication.BackendCommunication (baseUrl, isStatusCodeOk, doGenericRequest)
import Functions.State (makeStateT)


-- ----------------------------------------------------------------------------

getBlob :: HexString ->  Aff (Either ProtocolError (AXW.Response ArrayBuffer))
-- getBlob :: HexString -> StateT AppState Aff (Either ProtocolError (AXW.Response ArrayBuffer))
getBlob hash = do
  let url = joinWith "/" [baseUrl, "blobs", show $ hash]
  doGenericRequest url GET [] Nothing RF.arrayBuffer

getDecryptedBlob :: forall a. DecodeJson a => HexString -> CryptoKey -> Aff (Either ProtocolError a)
getDecryptedBlob reference key = runExceptT $ do
  response <- ExceptT $ getBlob reference
  if isStatusCodeOk response.status
    then withExceptT (\e -> CryptoError $ show e) (ExceptT $ decryptJson key response.body)
    else except $ Left (ResponseError (unwrap response.status))
-- getDecryptedBlob :: forall a. DecodeJson a => HexString -> CryptoKey -> StateT AppState Aff (Either ProtocolError a)
-- getDecryptedBlob reference key = mapStateT $ runExceptT $ do
--   response <- mapStateT ExceptT $ getBlob reference
--   makeStateT $ if isStatusCodeOk response.status
--                then withExceptT (\e -> CryptoError $ show e) (ExceptT $ decryptJson key response.body)
--                else except $ Left (ResponseError (unwrap response.status))

postBlob :: ArrayBuffer -> Aff (Either ProtocolError (AXW.Response String))
-- postBlob :: ArrayBuffer -> StateT AppState Aff (Either ProtocolError (AXW.Response String))
postBlob blob = do
  let url = joinWith "/" [baseUrl, "blobs"]
  let body = json $ encodeJson (fromArrayBuffer blob) :: RequestBody
  doGenericRequest url POST [] (Just body) RF.string
