module Functions.Communication.Users where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Alt ((<#>), (<$>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonCodec, encode)
import Data.Function ((#), ($))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, fromArrayBuffer, splitHexInHalf, toArrayBuffer, toString)
import Data.Identifier (computeIdentifier)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (class Show, show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (AppState, InvalidStateError(..), ProxyResponse(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRPVersions.CurrentSRPVersions (currentSRPVersion)
import DataModel.SRPVersions.SRP (HashFunction)
import DataModel.UserVersions.CurrentUserVersions (currentMasterKeyEncodingVersion, currentUserInfoCodecVersion)
import DataModel.UserVersions.User (class UserInfoVersions, MasterKey, MasterKeyEncodingVersion(..), RequestUserCard(..), UserCard(..), UserInfo(..), UserInfoReferences, UserPreferences, toUserInfo, userCardCodec)
import DataModel.UserVersions.UserCodecs (userInfoV1Codec, userInfoV2Codec, fromUserInfo)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.Communication.Backend (ConnectionState, genericRequest, isStatusCodeOk)
import Functions.Communication.Blobs (deleteBlob, getBlob, postBlob)
import Functions.EncodeDecode (decryptArrayBuffer, decryptJson, encryptArrayBuffer, encryptJson, exportCryptoKeyToHex, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)
import Functions.SRP (prepareV)

computeRemoteUserCard :: AppState -> ExceptT AppError Aff RequestUserCard
computeRemoteUserCard { c: Just c, p: Just p, s: Just s, masterKey: Just masterKey, srpConf } = do
  v <- withExceptT (show >>> SRPError >>> ProtocolError) $ ExceptT (prepareV srpConf (toArrayBuffer s) (toArrayBuffer p))
  pure $ RequestUserCard { c, v, s, srpVersion: currentSRPVersion, originMasterKey: Nothing, masterKey }
computeRemoteUserCard _ = throwError $ InvalidStateError (CorruptedState "computeRemoteUserCard")

updateUserCard :: ConnectionState -> HexString -> UserCard -> ExceptT AppError Aff (ProxyResponse Unit)
updateUserCard connectionState c newUserCard = do
  let url = joinWith "/" ["users", toString Hex c]
  let body = (json $ encode userCardCodec newUserCard) :: RequestBody
  ProxyResponse proxy' response <- genericRequest connectionState url PATCH (Just body) RF.ignore
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy' unit
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

deleteUserCard :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse Unit)
deleteUserCard connectionState c = do
  let url = joinWith "/" ["users", toString Hex c]
  ProxyResponse proxy response <- genericRequest connectionState url DELETE Nothing RF.string
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy unit
    else throwError (ProtocolError $ ResponseError $ unwrap response.status)

-- ------------

computeMasterKey :: UserInfoReferences -> CryptoKey -> Aff MasterKey
computeMasterKey {reference: userInfoHash, key: userInfoKey} masterPassword = do
  unencryptedMasterKeyContent <- concatArrayBuffers ((userInfoHash : userInfoKey : Nil) <#> toArrayBuffer) # liftEffect
  encryptedMasterKeyContent   <- encryptArrayBuffer masterPassword unencryptedMasterKeyContent
  pure $ Tuple (fromArrayBuffer encryptedMasterKeyContent) currentMasterKeyEncodingVersion

extractUserInfoReference :: MasterKey -> CryptoKey -> ExceptT AppError Aff UserInfoReferences
extractUserInfoReference (Tuple masterKeyContent masterKeyEncodingVersion) masterPassword = do
  case masterKeyEncodingVersion of
   MasterKeyEncodingVersion_1 -> decryptArrayBufferWithSplit
   MasterKeyEncodingVersion_2 -> decryptArrayBufferWithSplit

  where
    decryptArrayBufferWithSplit = do
      decryptedMasterKeyContent                      <- ExceptT $ decryptArrayBuffer masterPassword (toArrayBuffer masterKeyContent) <#> lmap (ProtocolError <<< CryptoError <<< show)
      let {before: userInfoHash, after: userInfoKey}  = splitHexInHalf (fromArrayBuffer decryptedMasterKeyContent)
      pure $ {reference: userInfoHash, key: userInfoKey}

decryptUserInfo :: ArrayBuffer -> HexString -> MasterKeyEncodingVersion -> ExceptT AppError Aff UserInfo
decryptUserInfo encryptedUserInfo key version =
  case version of 
    MasterKeyEncodingVersion_1 -> decryptJsonUserInfo userInfoV1Codec
    MasterKeyEncodingVersion_2 -> decryptJsonUserInfo userInfoV2Codec

  where
    decryptJsonUserInfo :: forall a. UserInfoVersions a => JsonCodec a -> ExceptT AppError Aff UserInfo
    decryptJsonUserInfo codec = (toUserInfo <$> ExceptT ((\cryptoKey -> decryptJson codec cryptoKey encryptedUserInfo) =<< (importCryptoKeyAesGCM (toArrayBuffer key)))) # mapError

    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (show >>> CryptoError >>> ProtocolError)

encryptUserInfo :: UserInfo -> HashFunction -> Aff (Tuple ArrayBuffer UserInfoReferences)
encryptUserInfo userInfo hashFunc = do
  userInfoKey           :: CryptoKey   <- generateCryptoKeyAesGCM
  encrytedUserInfo      :: ArrayBuffer <- encryptJson currentUserInfoCodecVersion userInfoKey (fromUserInfo userInfo)
  userInfoKeyHex        :: HexString   <- exportCryptoKeyToHex userInfoKey
  encryptedUserInfoHash :: HexString   <- hashFunc (encrytedUserInfo : Nil) <#> fromArrayBuffer
  pure $ Tuple encrytedUserInfo {reference: encryptedUserInfoHash, key: userInfoKeyHex}

getUserInfo :: ConnectionState -> UserInfoReferences -> MasterKeyEncodingVersion -> ExceptT AppError Aff (ProxyResponse UserInfo)
getUserInfo connectionState {reference, key} masterkeyEncodingVersion = do
  ProxyResponse proxy blob <- getBlob connectionState reference
  userInfo <- decryptUserInfo blob key masterkeyEncodingVersion
  pure $ ProxyResponse proxy userInfo

deleteUserInfo :: ConnectionState -> UserInfo -> UserInfoReferences -> ExceptT AppError Aff (ProxyResponse Unit)
deleteUserInfo connectionState (UserInfo {identifier}) {reference} = do
  deleteBlob connectionState reference identifier

postUserInfo :: ConnectionState -> UserInfo -> ExceptT AppError Aff (ProxyResponse UserInfoReferences)
postUserInfo connectionState@{hashFunc} userInfo@(UserInfo {identifier}) = do
  Tuple encryptedUserInfo userInfoReference@{reference} <- liftAff $ encryptUserInfo userInfo hashFunc
  ProxyResponse proxy _                                 <- postBlob connectionState encryptedUserInfo reference identifier
  pure $ ProxyResponse proxy userInfoReference


type UpdateUserStateUpdateInfo = {userInfo :: Maybe UserInfo, masterKey :: Maybe MasterKey, userInfoReferences :: Maybe UserInfoReferences }

updateUserInfo :: AppState -> UserInfo -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateUserInfo { c: Just c, p: Just p, masterKey: Just (Tuple originMasterKey _), proxy, hash: hashFunc, userInfo: Just oldUserInfo, userInfoReferences: Just userInfoReferences, srpConf } (UserInfo userInfo) = do
  let connectionState = {proxy, hashFunc, srpConf, c, p}
  
  newUserInfo         :: UserInfo              <- liftAff $ (\id -> UserInfo userInfo {identifier = id}) <$> computeIdentifier
  
  ProxyResponse proxy'   newUserInfoReferences <-           postUserInfo connectionState newUserInfo
  ProxyResponse proxy''  _                     <-           deleteUserInfo connectionState {proxy = proxy'} oldUserInfo userInfoReferences

  newMasterKey        :: MasterKey             <- liftAff $ computeMasterKey newUserInfoReferences =<< (importCryptoKeyAesGCM (toArrayBuffer p) # liftAff)
  ProxyResponse proxy''' _                     <-           updateUserCard connectionState{proxy = proxy''} c (UserCard { masterKey: newMasterKey, originMasterKey })

  pure $ ProxyResponse proxy''' { userInfo: Just newUserInfo, masterKey: Just newMasterKey, userInfoReferences: Just newUserInfoReferences }
  
updateUserInfo _ _ = 
  throwError $ InvalidStateError (MissingValue "Corrupted State")

updateUserPreferences :: AppState -> UserPreferences -> ExceptT AppError Aff (ProxyResponse UpdateUserStateUpdateInfo)

updateUserPreferences state@{ userInfo: Just (UserInfo userInfo) } newUserPreferences = do
  updateUserInfo state (UserInfo userInfo {userPreferences = newUserPreferences})

updateUserPreferences _ _ =
  throwError $ InvalidStateError (MissingValue "Missing user info")