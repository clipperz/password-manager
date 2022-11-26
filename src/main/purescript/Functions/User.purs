module Functions.User where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (decrypt, encrypt, raw, unwrapKey, CryptoKey)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Bifunctor (lmap, bimap)
import Data.Either (note, Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer, toArrayBuffer)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Traversable (sequence)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppState, AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..), CardEntry(..))
import DataModel.User (UserCard(..), IndexReference(..), UserInfoReferences(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EX
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.Blobs (deleteBlob)
import Functions.Communication.Cards (deleteCard)
import Functions.Communication.Users (getUserCard, deleteUserCard)
import Functions.EncodeDecode (encryptJson, decryptJson)
import Functions.JSState (getAppState)
import Functions.SRP as SRP
import Functions.State (getSRPConf)

type ModifyUserData = { c :: HexString
                      , oldUserCard :: UserCard
                      , newUserCard :: UserCard
                      }

changeUserPassword :: String -> String -> ExceptT AppError Aff Unit
changeUserPassword username password = do
  conf <- ExceptT $ liftEffect getSRPConf
  appState <- ExceptT $ liftEffect getAppState
  -- oldC <- except $ note (InvalidStateError $ MissingValue $ "c not present") $ toArrayBuffer <$> appState.c
  oldP <- except $ note (InvalidStateError $ MissingValue $ "p not present") $ toArrayBuffer <$> appState.p
  newC <- ExceptT $ Right <$> (SRP.prepareC conf username password)
  newP <- ExceptT $ Right <$> (SRP.prepareP conf username password)
  oldUserCard@(UserCard oldRecord) <- getUserCard
  newV <- ExceptT $ (lmap (ProtocolError <<< SRPError <<< show)) <$> (SRP.prepareV conf (toArrayBuffer oldRecord.s) newP)
  oldMasterPassword <- ExceptT $ Right <$> KI.importKey raw oldP (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  newMasterPassword <- ExceptT $ Right <$> KI.importKey raw newP (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  masterKeyDecryptedContent <- ExceptT $ (bimap (ProtocolError <<< CryptoError <<< show) UserInfoReferences) <$> decryptJson oldMasterPassword (toArrayBuffer oldRecord.masterKeyContent)
  masterKeyEncryptedContent <- ExceptT $ (Right <<< fromArrayBuffer) <$> encryptJson newMasterPassword masterKeyDecryptedContent
  let newUserCard = UserCard $ oldRecord { c = fromArrayBuffer newC, v = newV, masterKeyContent = masterKeyEncryptedContent }
  let modifyData = { c: oldRecord.c, oldUserCard, newUserCard }
  let url = joinWith "/" [ "users", show oldRecord.c ]
  let body = (json $ encodeJson modifyData) :: RequestBody
  response :: AXW.Response String <- manageGenericRequest url PUT (Just body) RF.string
  except $ if isStatusCodeOk response.status
           then Right unit
           else Left (ProtocolError (ResponseError (unwrap response.status)))

deleteUser :: Index -> ExceptT AppError Aff Unit
deleteUser (Index entries) = do
  (IndexReference record) <- ExceptT $ extractIndexReference <$> (liftEffect getAppState)
  _ <- sequence $ (deleteCard <<< entryToReference) <$> entries -- TODO: update index after every delete, to avoid dangling cards in case of failure?
  _ <- deleteBlob record.reference
  deleteUserCard

  where
    entryToReference (CardEntry r) = r.cardReference

    extractIndexReference :: Either AppError AppState -> Either AppError IndexReference
    extractIndexReference (Left err) = Left err
    extractIndexReference (Right state) = (\(UserInfoReferences { indexReference }) -> indexReference) <$> (note (InvalidStateError $ MissingValue $ "indexReference not present") state.userInfoReferences)

decryptUserInfoReferences :: HexString -> ExceptT AppError Aff UserInfoReferences
decryptUserInfoReferences encryptedRef = do
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: _, p: Just p, proxy: _, sessionKey: _, toll: _ } -> do
      masterPassword :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      mapCryptoError $ ExceptT $ decryptJson masterPassword (toArrayBuffer encryptedRef)
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing p"

  where 
    mapCryptoError :: forall a. ExceptT EX.Error Aff a -> ExceptT AppError Aff a
    mapCryptoError = withExceptT (\e -> ProtocolError $ CryptoError $ "Decrypt UserInfoReferences: " <> EX.message e)
