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
import DataModel.User (UserCard(..), IndexReference(..), UserPreferences, UserInfoReferences(..), UserPreferencesReference(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.Blobs (postBlob, getBlob, deleteBlob, getDecryptedBlob)
import Functions.EncodeDecode (encryptJson)
import Functions.Index (getIndexContent)
import Functions.JSState (getAppState, modifyAppState, updateAppState)
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
    { userInfoReferences: Just (UserInfoReferences { indexReference: indexRef@(IndexReference { reference }) }) } -> do
      blob <- getBlob reference
      getIndexContent blob indexRef
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing index reference"
  
getUserPreferences :: ExceptT AppError Aff UserPreferences
getUserPreferences = do 
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { userInfoReferences: Just (UserInfoReferences { preferencesReference: (UserPreferencesReference { reference, key }) }) } -> do
      cryptoKey     :: CryptoKey   <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      getDecryptedBlob reference cryptoKey
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing user preferences reference"

updateIndex :: Index -> ExceptT AppError Aff Unit
updateIndex newIndex = do
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: Just c, p: Just p, userInfoReferences: Just (UserInfoReferences r@{ indexReference: (IndexReference oldReference) })  } -> do
      UserCard userCard <- getUserCard
      masterPassword       :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      cryptoKey            :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer oldReference.masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      indexCardContent     :: ArrayBuffer <- ExceptT $ Right <$> encryptJson cryptoKey newIndex
      indexCardContentHash :: ArrayBuffer <- ExceptT $ Right <$> (getHashFromState $ currentState.hash) (indexCardContent : Nil)
      let newIndexReference = IndexReference $ oldReference { reference = fromArrayBuffer indexCardContentHash }
      let newInfoReference = UserInfoReferences $ r { indexReference = newIndexReference }
      masterKeyContent <- ExceptT $ (fromArrayBuffer >>> Right) <$> encryptJson masterPassword newInfoReference
      let newUserCard = UserCard $ userCard { masterKeyContent = masterKeyContent }
      _ <- postBlob indexCardContent indexCardContentHash
      let url = joinWith "/" ["users", show c]
      let body = (json $ encodeJson {c: c, oldUserCard: userCard, newUserCard: newUserCard}) :: RequestBody
      _ <- manageGenericRequest url PUT (Just body) RF.string
      _ <- deleteBlob oldReference.reference -- TODO: manage errors
      ExceptT $ updateAppState { userInfoReferences: Just newInfoReference}
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing p, c or indexReference"


updateUserPreferences :: UserPreferences -> ExceptT AppError Aff Unit
updateUserPreferences newUP = do
  -- (UserCard userCard) <- getUserCard
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { p: Just p, userInfoReferences: Just (UserInfoReferences r@{ preferencesReference: (UserPreferencesReference { reference, key }) }) } -> do
      UserCard userCard <- getUserCard
      cryptoKey     :: CryptoKey   <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      preferencesContent     :: ArrayBuffer <- ExceptT $ Right <$> encryptJson cryptoKey newUP
      preferencesContentHash :: ArrayBuffer <- ExceptT $ Right <$> (getHashFromState $ currentState.hash) (preferencesContent : Nil)
      let newReference = UserPreferencesReference { reference: fromArrayBuffer preferencesContentHash, key}
      let newInfoReference = UserInfoReferences $ r { preferencesReference = newReference }
      masterPassword       :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      masterKeyContent <- ExceptT $ (fromArrayBuffer >>> Right) <$> encryptJson masterPassword newInfoReference
      let newUserCard = UserCard $ userCard { masterKeyContent = masterKeyContent }
      -- save new preferences
      _ <- postBlob preferencesContent preferencesContentHash
      -- save new user card
      let url = joinWith "/" ["users", show userCard.c]
      let body = (json $ encodeJson {c: userCard.c, oldUserCard: userCard, newUserCard: newUserCard}) :: RequestBody
      _ <- manageGenericRequest url PUT (Just body) RF.string
      -- delete olf preferences
      _ <- deleteBlob reference
      -- update state      
      ExceptT $ updateAppState { userInfoReferences: Just newInfoReference }
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing user preferences reference"
