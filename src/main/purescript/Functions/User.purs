module Functions.User where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Affjax.Web as AXW
import Control.Alt (class Alt)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (ExceptT(..), except, mapExceptT, runExceptT, throwError, withExceptT)
import Control.Semigroupoid ((<<<))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (decrypt, encrypt, raw, unwrapKey, CryptoKey)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (toUnfoldable)
import Data.Bifunctor (lmap, bimap)
import Data.Either (note, Either(..))
import Data.Function (($))
import Data.Functor ((<$>), map, void)
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, fromArrayBuffer, toArrayBuffer, toString)
import Data.List (List, length, singleton, zipWith, (..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Operation (OperationStep(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppState, AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (Index(..), CardEntry(..))
import DataModel.User (IndexReference(..), MasterKeyEncodingVersion(..), RequestUserCard(..), SRPVersion(..), UserCard, UserInfoReferences(..), UserPreferencesReference(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff, class MonadAff)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Exception as EX
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.Blobs (deleteBlob)
import Functions.Communication.Cards (deleteCard)
import Functions.Communication.Users (getMasterKey, deleteUserCard)
import Functions.EncodeDecode (decryptJson, encryptJson)
import Functions.JSState (getAppState, updateAppState)
import Functions.Pin (deleteCredentials)
import Functions.SRP as SRP
import Functions.State (getSRPConf)
import Web.HTML (window)
import Web.HTML.Window (localStorage)

type ModifyUserData = { c :: HexString
                      , oldUserCard :: UserCard
                      , newUserCard :: UserCard
                      }

changeUserPassword :: String -> String -> ExceptT AppError Aff Unit
changeUserPassword username password = do
  conf     <- ExceptT $ liftEffect getSRPConf
  appState <- ExceptT $ liftEffect getAppState
  oldP <- except $ note (InvalidStateError $ MissingValue $ "p not present") $ toArrayBuffer <$> appState.p
  oldC <- except $ note (InvalidStateError $ MissingValue $ "c not present") appState.c
  s    <- liftAff $ SRP.randomArrayBuffer 32
  newC <- liftAff $ SRP.prepareC conf username password
  newP <- liftAff $ SRP.prepareP conf username password
  (Tuple masterKeyContent _) <- getMasterKey
  newV <- ExceptT $ (lmap (ProtocolError <<< SRPError <<< show)) <$> (SRP.prepareV conf s newP)
  oldMasterPassword <- ExceptT $ Right <$> KI.importKey raw oldP (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  newMasterPassword <- ExceptT $ Right <$> KI.importKey raw newP (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  masterKeyDecryptedContent <- ExceptT $ (bimap (ProtocolError <<< CryptoError <<< show) UserInfoReferences) <$> decryptJson oldMasterPassword (toArrayBuffer $ masterKeyContent)
  masterKeyEncryptedContent <- ExceptT $ (Right <<< fromArrayBuffer) <$> encryptJson newMasterPassword masterKeyDecryptedContent
  let newUserCard = RequestUserCard $ { c: fromArrayBuffer newC, v: newV, s: fromArrayBuffer s, masterKey: Tuple masterKeyEncryptedContent V_1, srpVersion : V_6a, originMasterKey: Just $ masterKeyContent }
  let url = joinWith "/" [ "users", toString Hex oldC ]
  let body = (json $ encodeJson newUserCard) :: RequestBody
  response :: AXW.Response Unit <- manageGenericRequest url PUT (Just body) RF.ignore
  if isStatusCodeOk response.status
    then ExceptT $ updateAppState {c : Just $ fromArrayBuffer newC, p: Just $ fromArrayBuffer newP, s: Just $ fromArrayBuffer s, username: Just username, password: Just password}
    else throwError $ ProtocolError (ResponseError (unwrap response.status))

deleteUserSteps :: forall m. MonadAff m 
                    => MonadEffect m 
                    => Alt m 
                    => Index 
                    -> (Int -> m (Either AppError Unit))
                    -> (String -> m (Either AppError Unit))
                    -> List (OperationStep (Either AppError Unit) (Either AppError Unit) m)
deleteUserSteps (Index entries) progressBarFunc stepPlaceholderFunc = do
  deleteCardStep <$> (zipWith (\i -> \c -> {index: i, entry: c}) (0 .. (length entries)) entries)
  <>
  (toUnfoldable $ createIntermediateSteps [ 
    Tuple  "Deleting index card" (do
      (IndexReference record) <- ExceptT $ extractIndexReference <$> (liftEffect getAppState)
      void $ deleteBlob record.reference
    )
  , Tuple "Deleting user preferences" (do
      (UserPreferencesReference record) <- ExceptT $ extractUserPreferencesReference <$> (liftEffect getAppState)
      void $ deleteBlob record.reference
    ) 
  , Tuple "Deleting user info" (do
          state <- ExceptT $ liftEffect getAppState
          (Tuple masterKeyContent masterKeyEncodingVersion) <- except $ (note (InvalidStateError $ MissingValue $ "masterKey not present") state.masterKey)
          case masterKeyEncodingVersion of
            V_1    -> pure unit -- with version 1.0 the userInfoReference record is saved inside the UserCard and not as a separate blob
            -- "2.0"   -> do
            --   p <- except $ (note (InvalidStateError $ MissingValue $ "p not present") state.p)
            --   masterPassword <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
            --   decryptedMasterKeyContent <- withExceptT (\err -> ProtocolError $ CryptoError $ show err) (ExceptT $ decryptWithAesCTR (toArrayBuffer masterKeyContent) masterPassword)
            --   { after: hash } <- pure $ splitHexInHalf (fromArrayBuffer decryptedMasterKeyContent)
            --   void $ deleteBlob hash
    ) 
  , Tuple "Deleting user card" (do
      c <- ExceptT $ extractC <$> (liftEffect getAppState)
      deleteUserCard c
    ) 
  ])
  <>
  (singleton $ LastStep (\psr ->
                case psr of
                  Left err -> pure $ Left err
                  Right _ -> liftAff $ runExceptT $ (ExceptT $ Right <$> (liftEffect (window >>= localStorage))) >>= (\v -> mapExceptT liftEffect (deleteCredentials v))
              ) (stepPlaceholderFunc "Deleting pin")) 

  where 
    deleteCardStep { index, entry: (CardEntry r) } = 
      IntermediateStep (\psr ->
                          case psr of
                            Left err -> pure $ Left err
                            Right _ -> liftAff $ ((<$>) (\_ -> unit)) <$> (runExceptT $ deleteCard $ r.cardReference)
                       ) (progressBarFunc index)

    createIntermediateSteps :: Array (Tuple String (ExceptT AppError Aff Unit)) -> Array (OperationStep (Either AppError Unit) (Either AppError Unit) m)
    createIntermediateSteps = map (\(Tuple placeholder stepActions) -> 
      IntermediateStep (\psr ->
                          case psr of
                            Left err -> pure $ Left err
                            Right _ -> liftAff $ runExceptT $ stepActions
                        ) (stepPlaceholderFunc placeholder)
    )

    extractIndexReference :: Either AppError AppState -> Either AppError IndexReference
    extractIndexReference (Left err)   = Left err
    extractIndexReference (Right state) = (\(UserInfoReferences { indexReference }) -> indexReference) <$> (note (InvalidStateError $ MissingValue $ "indexReference not present") state.userInfoReferences)

    extractUserPreferencesReference :: Either AppError AppState -> Either AppError UserPreferencesReference
    extractUserPreferencesReference (Left err)    = Left err
    extractUserPreferencesReference (Right state) = (\(UserInfoReferences { preferencesReference }) -> preferencesReference) <$> (note (InvalidStateError $ MissingValue $ "preferencesReference not present") state.userInfoReferences)

    extractC :: Either AppError AppState -> Either AppError HexString
    extractC (Left err)    = Left err
    extractC (Right state) = note (InvalidStateError $ MissingValue $ "c not present") state.c

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
