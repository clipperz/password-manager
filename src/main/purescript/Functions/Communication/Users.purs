module Functions.Communication.Users where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey, exportKey, raw)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (encode)
import Data.EuclideanRing ((/))
import Data.Function ((#), ($))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, fromArrayBuffer, splitHexInHalf, toArrayBuffer, toString)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..), fst)
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (InvalidStateError(..), ProxyResponse(..), AppState)
import DataModel.Codec as Codec
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..))
import DataModel.SRPCodec as SRPCodec
import DataModel.User (IndexReference(..), MasterKey, MasterKeyEncodingVersion(..), RequestUserCard(..), SRPVersion(..), UserCard(..), UserInfo(..), UserInfoReferences, UserPreferences)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.Communication.Backend (ConnectionState, genericRequest, isStatusCodeOk)
import Functions.Communication.Blobs (deleteBlob, getBlob, getDecryptedBlob, postBlob)
import Functions.EncodeDecode (decryptArrayBuffer, encryptArrayBuffer, encryptJson, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)
import Functions.Index (getIndexContent)
import Functions.SRP (prepareV, randomArrayBuffer)

computeMasterKey :: ArrayBuffer -> CryptoKey -> CryptoKey -> Aff MasterKey
computeMasterKey userInfoHash userInfoKey masterPassword = do
  arrayBufferKey              <- exportKey raw userInfoKey
  unencryptedMasterKeyContent <- concatArrayBuffers (userInfoHash : arrayBufferKey : Nil) # liftEffect
  encryptedMasterKeyContent   <- encryptArrayBuffer masterPassword unencryptedMasterKeyContent
  pure $ Tuple (fromArrayBuffer encryptedMasterKeyContent) V_1

extractUserInfoReference :: MasterKey -> CryptoKey -> ExceptT AppError Aff UserInfoReferences
extractUserInfoReference (Tuple masterKeyContent _) masterPassword = do
  decryptedMasterKeyContent                      <- ExceptT $ decryptArrayBuffer masterPassword (toArrayBuffer masterKeyContent) <#> lmap (ProtocolError <<< CryptoError <<< show)
  let {before: userInfoHash, after: userInfoKey}  = splitHexInHalf (fromArrayBuffer decryptedMasterKeyContent)
  pure $ Tuple userInfoHash userInfoKey

computeRemoteUserCard :: AppState -> ExceptT AppError Aff RequestUserCard
computeRemoteUserCard { c: Just c, p: Just p, s: Just s, masterKey: Just masterKey, srpConf } = do
  v <- withExceptT (show >>> SRPError >>> ProtocolError) $ ExceptT (prepareV srpConf (toArrayBuffer s) (toArrayBuffer p))
  pure $ RequestUserCard { c, v, s, srpVersion: V_6a, originMasterKey: Nothing, masterKey }
computeRemoteUserCard _ = throwError $ InvalidStateError (CorruptedState "State is corrupted")

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

getUserInfo :: ConnectionState -> UserInfoReferences -> ExceptT AppError Aff (ProxyResponse UserInfo)
getUserInfo connectionState (Tuple reference key) = do
  cryptoKey :: CryptoKey <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer key)
  getDecryptedBlob connectionState reference Codec.userInfoCodec cryptoKey

-- ------------

deleteUserInfo :: ConnectionState -> UserInfo -> MasterKey -> ExceptT AppError Aff (ProxyResponse Unit)
deleteUserInfo connectionState@{p} (UserInfo {identifier}) masterKey = do
  masterPassword    <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer p)
  Tuple reference _ <-           extractUserInfoReference masterKey masterPassword
  deleteBlob connectionState reference identifier

updateUserCard :: ConnectionState -> HexString -> UserCard -> ExceptT AppError Aff (ProxyResponse Unit)
updateUserCard connectionState c newUserCard = do
  let url = joinWith "/" ["users", toString Hex c]
  let body = (json $ encode SRPCodec.userCardCodec newUserCard) :: RequestBody
  ProxyResponse proxy' response <- genericRequest connectionState url PATCH (Just body) RF.ignore
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy' unit
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

type UpdateUserStateUpdateInfo = {newUserInfo :: UserInfo, newMasterKey :: MasterKey }

updateUserInfo :: AppState -> UserInfo -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateUserInfo { c: Just c, p: Just p, masterKey: Just originMasterKey, proxy, hash: hashFunc, userInfo: Just (UserInfo oldUserInfo), srpConf } (UserInfo userInfo) = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  
  newUserInfoIdentifier :: HexString   <- liftAff $ randomArrayBuffer (256/8) <#> fromArrayBuffer
  let newUserInfo        = UserInfo                 userInfo {identifier = newUserInfoIdentifier}
  
  userInfoKey           :: CryptoKey   <- liftAff $ generateCryptoKeyAesGCM
  encryptedUserInfo     :: ArrayBuffer <- liftAff $ encryptJson Codec.userInfoCodec userInfoKey newUserInfo
  userInfoHash          :: ArrayBuffer <- liftAff $ hashFunc (encryptedUserInfo : Nil)
  ProxyResponse proxy'       _         <-           postBlob connectionState encryptedUserInfo (fromArrayBuffer userInfoHash) newUserInfoIdentifier

  masterPassword        :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer p)

  Tuple oldUserInfoReference _         <-           extractUserInfoReference originMasterKey masterPassword
  ProxyResponse proxy''      _         <-           deleteBlob connectionState{proxy = proxy'} oldUserInfoReference oldUserInfo.identifier

  newMasterKey          :: MasterKey   <- liftAff $ computeMasterKey userInfoHash userInfoKey masterPassword
  ProxyResponse proxy'''     _         <-           updateUserCard connectionState{proxy = proxy''} c (UserCard { masterKey: newMasterKey, originMasterKey: fst originMasterKey })

  pure $ ProxyResponse proxy''' { newUserInfo, newMasterKey }
  
updateUserInfo _ _ = 
  throwError $ InvalidStateError (MissingValue "Corrupted State")

updateIndex :: AppState -> Index -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateIndex state@{ c: Just c, p: Just p, proxy, hash: hashFunc, userInfo: Just (UserInfo userInfo@{indexReference: IndexReference oldReference}), index: Just (Index oldIndex), srpConf } (Index newIndex) = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}

  newIndexCardIdentifier <- liftAff $ randomArrayBuffer (256/8) <#> fromArrayBuffer

  cryptoKey            :: CryptoKey   <- liftAff $ generateCryptoKeyAesGCM
  indexCardContent     :: ArrayBuffer <- liftAff $ encryptJson Codec.indexCodec cryptoKey (Index newIndex {identifier = newIndexCardIdentifier})
  indexCardContentHash :: ArrayBuffer <- liftAff $ hashFunc (indexCardContent : Nil)
  ProxyResponse proxy'   _            <-           postBlob connectionState indexCardContent (fromArrayBuffer indexCardContentHash) newIndexCardIdentifier
  let newIndexReference = IndexReference         $ oldReference { reference = fromArrayBuffer indexCardContentHash }
  -- -------------------
  ProxyResponse proxy'' _             <-           deleteBlob connectionState{proxy = proxy'} oldReference.reference oldIndex.identifier
  -- -------------------
  updateUserInfo (state {proxy = proxy''}) (UserInfo userInfo { indexReference = newIndexReference })

updateIndex _ _ = 
  throwError $ InvalidStateError (MissingValue "Corrupted State")

updateUserPreferences :: AppState -> UserPreferences -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateUserPreferences state@{ userInfo: Just (UserInfo userInfo) } newUserPreferences = do
  updateUserInfo state (UserInfo userInfo {userPreferences = newUserPreferences})

updateUserPreferences _ _ =
  throwError $ InvalidStateError (MissingValue "Missing user info")