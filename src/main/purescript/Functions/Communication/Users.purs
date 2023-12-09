module Functions.Communication.Users
  ( UpdateUserStateUpdateInfo
  , deleteUserCard
  , getIndexWithState
  , getMasterKey
  , getRemoteUserCard
  , getIndex
  , getStatelessMasterKey
  , getStatelessUserPreferences
  , getUserPreferences
  , updateIndex
  , updateUserCard
  , updateUserPreferences
  )
  where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Alt ((<#>))
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
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import DataModel.User (IndexReference(..), MasterKeyEncodingVersion(..), RequestUserCard(..), SRPVersion(..), UserCard(..), UserInfoReferences(..), UserPreferences, UserPreferencesReference(..), MasterKey)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.Blobs (deleteStatelessBlob, getBlob, getDecryptedBlob, getStatelessBlob, getStatelessDecryptedBlob, postStatelessBlob)
import Functions.Communication.StatelessBackend (ConnectionState)
import Functions.Communication.StatelessBackend as Stateless
import Functions.EncodeDecode (cryptoKeyAES, encryptJson)
import Functions.Index (getIndexContent)
import Functions.JSState (getAppState, updateAppState)
import Functions.SRP (prepareV)
import Functions.State (getSRPConf)

getStatelessMasterKey :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse MasterKey)
getStatelessMasterKey connectionState c = do
  let url = joinWith "/" ["users", show c]
  ProxyResponse newProxy response <- Stateless.manageGenericRequest connectionState url GET Nothing RF.json
  if isStatusCodeOk response.status
    then do
      newMasterKey <- except $ flip lmap (decodeJson response.body) (show >>> DecodeError >>> ProtocolError)
      pure $ ProxyResponse newProxy newMasterKey
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)

-- TODO REMOVE
getMasterKey :: HexString -> ExceptT AppError Aff MasterKey
getMasterKey c = do
  { masterKey: maybeMasterKey } <- ExceptT $ liftEffect $ getAppState
  case maybeMasterKey of
    Nothing -> do
      -- c <- except $ note (InvalidStateError (MissingValue "c is Nothing")) maybec
      let url = joinWith "/" ["users", show c]
      response <- manageGenericRequest url GET Nothing RF.json
      if isStatusCodeOk response.status
        then do
          newMasterKey <- except $ flip lmap (decodeJson response.body) (show >>> DecodeError >>> ProtocolError)
          ExceptT $ updateAppState { masterKey: Just newMasterKey }
          pure newMasterKey
        else throwError $ ProtocolError (ResponseError $ unwrap response.status)
    Just masterKey -> pure masterKey
-- ------------

getRemoteUserCard :: ExceptT AppError Aff RequestUserCard
getRemoteUserCard = do
  state <- ExceptT $ liftEffect $ getAppState
  case state of
    { c: Just c, p: Just p, s: Just s, masterKey: Just masterKey } -> do
      srpConf <- ExceptT $ liftEffect getSRPConf
      v       <- withExceptT (show >>> SRPError >>> ProtocolError) $ ExceptT (prepareV srpConf (toArrayBuffer s) (toArrayBuffer p))
      pure $ RequestUserCard { c, v, s, srpVersion: V_6a, originMasterKey: Nothing, masterKey }
    _ -> throwError $ InvalidStateError (MissingValue "c, s or masterKey are Nothing")

updateUserCard :: ConnectionState -> HexString -> UserCard -> ExceptT AppError Aff (ProxyResponse MasterKey)
updateUserCard connectionState c newUserCard = do
  let url = joinWith "/" ["users", toString Hex c]
  let body = (json $ encodeJson newUserCard) :: RequestBody
  ProxyResponse proxy' response <- Stateless.manageGenericRequest connectionState url PATCH (Just body) RF.string
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy' (unwrap newUserCard).masterKey
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

deleteUserCard :: HexString -> ExceptT AppError Aff Unit
deleteUserCard c = do
  let url = joinWith "/" ["users", toString Hex c]
  response <- manageGenericRequest url DELETE Nothing RF.string
  if isStatusCodeOk response.status
    then pure unit
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

-- TODO REMOVE
getIndexWithState :: ExceptT AppError Aff Index
getIndexWithState = do 
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { userInfoReferences: Just (UserInfoReferences { indexReference: indexRef@(IndexReference { reference }) }) } -> do
      blob <- getBlob reference
      getIndexContent blob indexRef
    _ -> throwError (InvalidStateError $ MissingValue "Missing index reference")
-- -------------

getIndex :: ConnectionState -> IndexReference -> ExceptT AppError Aff (ProxyResponse Index)
getIndex connectionState indexRef@(IndexReference { reference }) = do
  ProxyResponse newProxy blob <- getStatelessBlob connectionState reference
  getIndexContent blob indexRef <#> ProxyResponse newProxy

-- TODO REMOVE
getUserPreferences :: ExceptT AppError Aff UserPreferences
getUserPreferences = do 
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { userInfoReferences: Just (UserInfoReferences { preferencesReference: (UserPreferencesReference { reference, key }) }) } -> do
      cryptoKey :: CryptoKey <- liftAff $ KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      getDecryptedBlob reference cryptoKey
    _ -> throwError (InvalidStateError $ MissingValue "Missing user preferences reference")
-- -----------

getStatelessUserPreferences :: ConnectionState -> UserPreferencesReference -> ExceptT AppError Aff (ProxyResponse UserPreferences)
getStatelessUserPreferences connectionState (UserPreferencesReference { reference, key }) = do
  cryptoKey :: CryptoKey <- liftAff $ KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  getStatelessDecryptedBlob connectionState reference cryptoKey

-- ------------

type UpdateUserStateUpdateInfo = {newUserInfoReferences :: UserInfoReferences, newMasterKey :: MasterKey }

updateIndex :: StatelessAppState -> Index -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateIndex { c: Just c, p: Just p, userInfoReferences: Just (UserInfoReferences r@{ indexReference: (IndexReference oldReference) }), masterKey: Just originMasterKey, proxy, hash: hashFunc, index: Just index } newIndex = do
  cryptoKey            :: CryptoKey   <- liftAff $ cryptoKeyAES (toArrayBuffer oldReference.masterKey)
  indexCardContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newIndex
  indexCardContentHash :: ArrayBuffer <- liftAff $ hashFunc (indexCardContent : Nil)
  ProxyResponse proxy'   _            <- postStatelessBlob {proxy, hashFunc} indexCardContent indexCardContentHash
  -- -------------------
  let newIndexReference                = IndexReference $ oldReference { reference = fromArrayBuffer indexCardContentHash }
  let newUserInfoReferences            = UserInfoReferences r { indexReference = newIndexReference }
  masterPassword       :: CryptoKey   <- liftAff $ cryptoKeyAES (toArrayBuffer p)
  masterKeyContent     :: HexString   <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword newUserInfoReferences
  let newUserCard                      = UserCard { masterKey: Tuple masterKeyContent V_1, originMasterKey: fst originMasterKey }
  ProxyResponse proxy'' newMasterKey  <- updateUserCard {proxy: proxy', hashFunc} c newUserCard
  -- -------------------
  oldIndexCartContent  :: ArrayBuffer <- liftAff $ encryptJson cryptoKey index

  ProxyResponse proxy''' _            <- deleteStatelessBlob {proxy: proxy'', hashFunc} oldIndexCartContent oldReference.reference
  
  pure $ ProxyResponse proxy''' { newUserInfoReferences, newMasterKey }

updateIndex _ _ = 
  throwError $ InvalidStateError (MissingValue "Missing p, c or indexReference")

updateUserPreferences :: StatelessAppState -> UserPreferences -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateUserPreferences { c: Just c, p: Just p, userInfoReferences: Just (UserInfoReferences r@{ preferencesReference: (UserPreferencesReference { reference, key }) }), masterKey: Just originMasterKey, proxy, hash: hashFunc, userPreferences: Just userPreferences } newUserPreferences = do
  cryptoKey              :: CryptoKey   <- liftAff $ KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  preferencesContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newUserPreferences
  preferencesContentHash :: ArrayBuffer <- liftAff $ hashFunc (preferencesContent : Nil)
  ProxyResponse proxy'   _              <- postStatelessBlob {proxy, hashFunc} preferencesContent preferencesContentHash
  -- -------------------
  let newUserPreferencesReference        = UserPreferencesReference { reference: fromArrayBuffer preferencesContentHash, key}
  let newUserInfoReferences              = UserInfoReferences r { preferencesReference = newUserPreferencesReference }
  masterPassword         :: CryptoKey   <- liftAff $ cryptoKeyAES (toArrayBuffer p)
  masterKeyContent       :: HexString   <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword newUserInfoReferences
  let newUserCard                        = UserCard { masterKey: Tuple masterKeyContent V_1, originMasterKey: fst originMasterKey }
  ProxyResponse proxy'' newMasterKey    <- updateUserCard {proxy: proxy', hashFunc} c newUserCard
  -- -------------------
  oldUserPreferencesCartContent         <- liftAff $ encryptJson cryptoKey userPreferences
  ProxyResponse proxy''' _              <- deleteStatelessBlob {proxy: proxy'', hashFunc} oldUserPreferencesCartContent reference

  pure $ ProxyResponse proxy''' { newUserInfoReferences, newMasterKey }

updateUserPreferences _ _ =
  throwError $ InvalidStateError (MissingValue "Missing user preferences reference")