module Functions.Communication.Users where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
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
import DataModel.AppError (AppError(..))
import DataModel.AppState (InvalidStateError(..), ProxyResponse(..), AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index)
import DataModel.User (IndexReference(..), MasterKeyEncodingVersion(..), RequestUserCard(..), SRPVersion(..), UserCard(..), UserInfoReferences(..), UserPreferences, UserPreferencesReference(..), MasterKey)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Communication.Backend (ConnectionState, genericRequest, isStatusCodeOk)
import Functions.Communication.Blobs (deleteBlob, getBlob, getDecryptedBlob, postBlob)
import Functions.EncodeDecode (cryptoKeyAES, encryptJson)
import Functions.Index (getIndexContent)
import Functions.SRP (prepareV)

getMasterKey :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse MasterKey)
getMasterKey connectionState c = do
  let url = joinWith "/" ["users", show c]
  ProxyResponse newProxy response <- genericRequest connectionState url GET Nothing RF.json
  if isStatusCodeOk response.status
    then do
      newMasterKey <- except $ flip lmap (decodeJson response.body) (show >>> DecodeError >>> ProtocolError)
      pure $ ProxyResponse newProxy newMasterKey
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)

computeRemoteUserCard :: AppState -> ExceptT AppError Aff RequestUserCard
computeRemoteUserCard { c: Just c, p: Just p, s: Just s, masterKey: Just masterKey, srpConf } = do
  v <- withExceptT (show >>> SRPError >>> ProtocolError) $ ExceptT (prepareV srpConf (toArrayBuffer s) (toArrayBuffer p))
  pure $ RequestUserCard { c, v, s, srpVersion: V_6a, originMasterKey: Nothing, masterKey }
computeRemoteUserCard _ = throwError $ InvalidStateError (CorruptedState "State is corrupted")

updateUserCard :: ConnectionState -> HexString -> UserCard -> ExceptT AppError Aff (ProxyResponse MasterKey)
updateUserCard connectionState c newUserCard = do
  let url = joinWith "/" ["users", toString Hex c]
  let body = (json $ encodeJson newUserCard) :: RequestBody
  ProxyResponse proxy' response <- genericRequest connectionState url PATCH (Just body) RF.string
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy' (unwrap newUserCard).masterKey
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

deleteUserCard :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse Unit)
deleteUserCard connectionState c = do
  let url = joinWith "/" ["users", toString Hex c]
  ProxyResponse proxy response <- genericRequest connectionState url DELETE Nothing RF.string
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy unit
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

getIndex :: ConnectionState -> IndexReference -> ExceptT AppError Aff (ProxyResponse Index)
getIndex connectionState indexRef@(IndexReference { reference }) = do
  ProxyResponse newProxy blob <- getBlob connectionState reference
  getIndexContent blob indexRef <#> ProxyResponse newProxy

getUserPreferences :: ConnectionState -> UserPreferencesReference -> ExceptT AppError Aff (ProxyResponse UserPreferences)
getUserPreferences connectionState (UserPreferencesReference { reference, key }) = do
  cryptoKey :: CryptoKey <- liftAff $ KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  getDecryptedBlob connectionState reference cryptoKey

-- ------------

deleteUserInfo :: ConnectionState -> MasterKey -> ExceptT AppError Aff (ProxyResponse Unit)
deleteUserInfo connectionState (Tuple _ masterKeyEncodingVersion) =
  case masterKeyEncodingVersion of
    V_1    -> pure $ ProxyResponse connectionState.proxy unit -- with version 1.0 the userInfoReference record is saved inside the UserCard and not as a separate blob
    -- "2.0"   -> do
    --   p <- except $ (note (InvalidStateError $ MissingValue $ "p not present") state.p)
    --   masterPassword <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
    --   decryptedMasterKeyContent <- withExceptT (\err -> ProtocolError $ CryptoError $ show err) (ExceptT $ decryptWithAesCTR (toArrayBuffer masterKeyContent) masterPassword)
    --   { after: hash } <- pure $ splitHexInHalf (fromArrayBuffer decryptedMasterKeyContent)
    --   void $ deleteBlob hash

type UpdateUserStateUpdateInfo = {newUserInfoReferences :: UserInfoReferences, newMasterKey :: MasterKey }

updateIndex :: AppState -> Index -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateIndex { c: Just c, p: Just p, userInfoReferences: Just (UserInfoReferences r@{ indexReference: (IndexReference oldReference) }), masterKey: Just originMasterKey, proxy, hash: hashFunc, index: Just index, username: Just username, password: Just password, srpConf } newIndex = do
  let connectionState = {proxy, hashFunc, srpConf, credentials: {username, password}}
  cryptoKey            :: CryptoKey   <- liftAff $ cryptoKeyAES (toArrayBuffer oldReference.masterKey)
  indexCardContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newIndex
  indexCardContentHash :: ArrayBuffer <- liftAff $ hashFunc (indexCardContent : Nil)
  ProxyResponse proxy'   _            <- postBlob connectionState indexCardContent indexCardContentHash
  -- -------------------
  let newIndexReference                = IndexReference $ oldReference { reference = fromArrayBuffer indexCardContentHash }
  let newUserInfoReferences            = UserInfoReferences r { indexReference = newIndexReference }
  masterPassword       :: CryptoKey   <- liftAff $ cryptoKeyAES (toArrayBuffer p)
  masterKeyContent     :: HexString   <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword newUserInfoReferences
  let newUserCard                      = UserCard { masterKey: Tuple masterKeyContent V_1, originMasterKey: fst originMasterKey }
  ProxyResponse proxy'' newMasterKey  <- updateUserCard connectionState{proxy = proxy'} c newUserCard
  -- -------------------
  oldIndexCartContent  :: ArrayBuffer <- liftAff $ encryptJson cryptoKey index

  ProxyResponse proxy''' _            <- deleteBlob connectionState{proxy = proxy''} oldIndexCartContent oldReference.reference
  
  pure $ ProxyResponse proxy''' { newUserInfoReferences, newMasterKey }

updateIndex _ _ = 
  throwError $ InvalidStateError (MissingValue "Missing p, c or indexReference")

updateUserPreferences :: AppState -> UserPreferences -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateUserPreferences { c: Just c, p: Just p, username: Just username, password: Just password, srpConf, userInfoReferences: Just (UserInfoReferences r@{ preferencesReference: (UserPreferencesReference { reference, key }) }), masterKey: Just originMasterKey, proxy, hash: hashFunc, userPreferences: Just userPreferences } newUserPreferences = do
  let connectionState = {proxy, hashFunc, srpConf, credentials: {username, password}}
  cryptoKey              :: CryptoKey   <- liftAff $ KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  preferencesContent     :: ArrayBuffer <- liftAff $ encryptJson cryptoKey newUserPreferences
  preferencesContentHash :: ArrayBuffer <- liftAff $ hashFunc (preferencesContent : Nil)
  ProxyResponse proxy'   _              <- postBlob connectionState preferencesContent preferencesContentHash
  -- -------------------
  let newUserPreferencesReference        = UserPreferencesReference { reference: fromArrayBuffer preferencesContentHash, key}
  let newUserInfoReferences              = UserInfoReferences r { preferencesReference = newUserPreferencesReference }
  masterPassword         :: CryptoKey   <- liftAff $ cryptoKeyAES (toArrayBuffer p)
  masterKeyContent       :: HexString   <- liftAff $ fromArrayBuffer <$> encryptJson masterPassword newUserInfoReferences
  let newUserCard                        = UserCard { masterKey: Tuple masterKeyContent V_1, originMasterKey: fst originMasterKey }
  ProxyResponse proxy'' newMasterKey    <- updateUserCard connectionState{proxy = proxy'} c newUserCard
  -- -------------------
  oldUserPreferencesCartContent         <- liftAff $ encryptJson cryptoKey userPreferences
  ProxyResponse proxy''' _              <- deleteBlob connectionState{proxy = proxy''} oldUserPreferencesCartContent reference

  pure $ ProxyResponse proxy''' { newUserInfoReferences, newMasterKey }

updateUserPreferences _ _ =
  throwError $ InvalidStateError (MissingValue "Missing user preferences reference")