module Functions.Communication.Users where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, except)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..), note)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (toArrayBuffer, fromArrayBuffer)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index)
import DataModel.User (UserCard(..), IndexReference(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.Blobs (postBlob, getBlob, deleteBlob)
import Functions.EncodeDecode (encryptJson)
import Functions.Index (getIndexContent)
import Functions.JSState (getAppState, modifyAppState)
import Functions.State (getHashFromState)

getUserCard :: ExceptT AppError Aff UserCard
getUserCard = do
  { proxy: _, c: mc, p: _, sessionKey: _, toll: _ } <- ExceptT $ liftEffect $ getAppState
  c <- except $ note (InvalidStateError (MissingValue "c is Nothing")) mc
  let url = joinWith "/" ["users", show c]
  response <- manageGenericRequest url GET Nothing RF.json
  if isStatusCodeOk response.status
    then withExceptT (\e -> ProtocolError (DecodeError (show e))) (except $ decodeJson response.body)
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

deleteUserCard :: ExceptT AppError Aff Unit
deleteUserCard = do
  uc@(UserCard userRecord) <- getUserCard
  let url = joinWith "/" ["users", show userRecord.c]
  let body = (json $ encodeJson uc) :: RequestBody
  response <- manageGenericRequest url DELETE (Just body) RF.string
  if isStatusCodeOk response.status
    then pure unit
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

getIndex :: ExceptT AppError Aff Index
getIndex = do 
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { indexReference: Just indexRef@(IndexReference { reference }) } -> do
      blob <- getBlob reference
      getIndexContent blob indexRef
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing index reference"

updateIndex :: Index -> ExceptT AppError Aff Unit
updateIndex newIndex = do
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: Just c, p: Just p, indexReference: Just (IndexReference oldReference) } -> do
      UserCard userCard <- getUserCard
      masterPassword       :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      cryptoKey            :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer oldReference.masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      indexCardContent     :: ArrayBuffer <- ExceptT $ Right <$> encryptJson cryptoKey newIndex
      indexCardContentHash :: ArrayBuffer <- ExceptT $ Right <$> (getHashFromState $ currentState.hash) (indexCardContent : Nil)
      let newIndexReference = IndexReference $ oldReference { reference = fromArrayBuffer indexCardContentHash }
      masterKeyContent <- ExceptT $ (fromArrayBuffer >>> Right) <$> encryptJson masterPassword newIndexReference
      let newUserCard = UserCard $ userCard { masterKeyContent = masterKeyContent }
      _ <- postBlob indexCardContent indexCardContentHash
      let url = joinWith "/" ["users", show c]
      let body = (json $ encodeJson {c: c, oldUserCard: userCard, newUserCard: newUserCard}) :: RequestBody
      _ <- manageGenericRequest url PUT (Just body) RF.string
      _ <- deleteBlob oldReference.reference -- TODO: manage errors
      ExceptT $ Right <$> (modifyAppState $ currentState { indexReference = Just newIndexReference})
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing p, c or indexReference"

