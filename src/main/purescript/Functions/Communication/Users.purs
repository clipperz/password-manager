module Functions.Communication.Users where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, throwError, withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Function (flip, ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, fromArrayBuffer, toArrayBuffer, toString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index)
import DataModel.User (IndexReference(..), MasterKeyEncodingVersion(..), RequestUserCard(..), SRPVersion(..), UserCard(..), UserInfoReferences(..), UserPreferences, UserPreferencesReference(..), MasterKey)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.Blobs (postBlob, getBlob, deleteBlob, getDecryptedBlob)
import Functions.EncodeDecode (encryptJson)
import Functions.Index (getIndexContent)
import Functions.JSState (getAppState, updateAppState)
import Functions.SRP (prepareV)
import Functions.State (getHashFromState, getSRPConf)

getMasterKey :: ExceptT AppError Aff MasterKey
getMasterKey = do
  { c: maybec, masterKey: maybeMasterKey } <- ExceptT $ liftEffect $ getAppState
  case maybeMasterKey of
    Nothing -> do
      c <- except $ note (InvalidStateError (MissingValue "c is Nothing")) maybec
      let url = joinWith "/" ["users", show c]
      response <- manageGenericRequest url GET Nothing RF.json
      if isStatusCodeOk response.status
        then do
          newMasterKey <- except $ flip lmap (decodeJson response.body) (show >>> DecodeError >>> ProtocolError)
          ExceptT $ updateAppState { masterKey: Just newMasterKey }
          pure newMasterKey
        else throwError $ ProtocolError (ResponseError $ unwrap response.status)
    Just masterKey -> pure masterKey

getRemoteUserCard :: ExceptT AppError Aff RequestUserCard
getRemoteUserCard = do
  state <- ExceptT $ liftEffect $ getAppState
  case state of
    { c: Just c, p: Just p, s: Just s, masterKey: Just masterKey } -> do
      srpConf <- ExceptT $ liftEffect getSRPConf
      v       <- withExceptT (show >>> SRPError >>> ProtocolError) $ ExceptT (prepareV srpConf (toArrayBuffer s) (toArrayBuffer p))
      pure $ RequestUserCard { c, v, s, srpVersion: V_6a, originMasterKey: Nothing, masterKey }
    _ -> throwError $ InvalidStateError (MissingValue "c, s or masterKey are Nothing")
  
updateUserCard :: HexString -> UserCard -> ExceptT AppError Aff Unit
updateUserCard c newUserCard@(UserCard userCardRecord) = do
  let url = joinWith "/" ["users", toString Hex c]
  let body = (json $ encodeJson newUserCard) :: RequestBody
  response <- manageGenericRequest url PATCH (Just body) RF.string
  if isStatusCodeOk response.status
    then do
      ExceptT $ updateAppState { masterKey: Just userCardRecord.masterKey }
      pure unit
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

deleteUserCard :: HexString -> ExceptT AppError Aff Unit
deleteUserCard c = do
  let url = joinWith "/" ["users", toString Hex c]
  response <- manageGenericRequest url DELETE Nothing RF.string
  if isStatusCodeOk response.status
    then pure unit
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

getIndex :: ExceptT AppError Aff Index
getIndex = do 
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { userInfoReferences: Just (UserInfoReferences { indexReference: indexRef@(IndexReference { reference }) }) } -> do
      blob <- getBlob reference
      getIndexContent blob indexRef
    _ -> throwError (InvalidStateError $ MissingValue "Missing index reference")

getUserPreferences :: ExceptT AppError Aff UserPreferences
getUserPreferences = do 
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { userInfoReferences: Just (UserInfoReferences { preferencesReference: (UserPreferencesReference { reference, key }) }) } -> do
      cryptoKey :: CryptoKey <- liftAff $ KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      getDecryptedBlob reference cryptoKey
    _ -> throwError (InvalidStateError $ MissingValue "Missing user preferences reference")

updateIndex :: Index -> ExceptT AppError Aff Unit
updateIndex newIndex = do
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: Just c, p: Just p, userInfoReferences: Just (UserInfoReferences r@{ indexReference: (IndexReference oldReference) })  } -> do
      
      cryptoKey            :: CryptoKey   <- liftAff $ KI.importKey raw (toArrayBuffer oldReference.masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      indexCardContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newIndex
      indexCardContentHash :: ArrayBuffer <- liftAff $ (getHashFromState $ currentState.hash) (indexCardContent : Nil)
      _ <- postBlob indexCardContent indexCardContentHash
      -- -------------------
      let newIndexReference = IndexReference $ oldReference { reference = fromArrayBuffer indexCardContentHash }
      let newInfoReference  = UserInfoReferences r { indexReference = newIndexReference }
      masterPassword       :: CryptoKey <- liftAff $ KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      masterKeyContent     :: HexString <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword newInfoReference
      originMasterKey      :: MasterKey <- getMasterKey
      let newUserCard       = UserCard { masterKey: Tuple masterKeyContent V_1, originMasterKey: fst originMasterKey }
      _ <- updateUserCard c newUserCard
      -- -------------------
      _ <- deleteBlob oldReference.reference -- TODO: manage errors
      
      ExceptT $ updateAppState { userInfoReferences: Just newInfoReference}
    
    _ -> throwError $ InvalidStateError (MissingValue "Missing p, c or indexReference")

updateUserPreferences :: UserPreferences -> ExceptT AppError Aff Unit
updateUserPreferences newUP = do
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: Just c, p: Just p, userInfoReferences: Just (UserInfoReferences r@{ preferencesReference: (UserPreferencesReference { reference, key }) }) } -> do
      
      cryptoKey              :: CryptoKey   <- liftAff $ KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      preferencesContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newUP
      preferencesContentHash :: ArrayBuffer <- liftAff $ (getHashFromState currentState.hash) (preferencesContent : Nil)
      _ <- postBlob preferencesContent preferencesContentHash
      -- -------------------
      let newReference     = UserPreferencesReference { reference: fromArrayBuffer preferencesContentHash, key}
      let newInfoReference = UserInfoReferences r { preferencesReference = newReference }
      masterPassword      :: CryptoKey <- liftAff $ KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      masterKeyContent    :: HexString <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword newInfoReference
      originMasterKey     :: MasterKey <- getMasterKey
      let newUserCard      = UserCard { masterKey: Tuple masterKeyContent V_1, originMasterKey: fst originMasterKey }
      _ <- updateUserCard c newUserCard
      -- -------------------
      _ <- deleteBlob reference

      ExceptT $ updateAppState { userInfoReferences: Just newInfoReference }
    
    _ -> throwError $ InvalidStateError (MissingValue "Missing user preferences reference")
