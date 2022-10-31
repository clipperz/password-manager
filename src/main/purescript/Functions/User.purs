module Functions.User where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Crypto.Subtle.Constants.AES (aesCTR, l256)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, exportKey, decrypt, raw, unwrapKey, CryptoKey)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), except)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Bifunctor (lmap, rmap, bimap)
import Data.Either (note, Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer, toArrayBuffer)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.User (UserCard(..), IndexReference(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.Cards (getUserCard)
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
  oldC <- except $ note (InvalidStateError $ MissingValue $ "c not present") $ toArrayBuffer <$> appState.c
  oldP <- except $ note (InvalidStateError $ MissingValue $ "p not present") $ toArrayBuffer <$> appState.p
  newC <- ExceptT $ Right <$> (SRP.prepareC conf username password)
  newP <- ExceptT $ Right <$> (SRP.prepareP conf username password)
  oldUserCard@(UserCard oldRecord) <- getUserCard
  newV <- ExceptT $ (lmap (ProtocolError <<< SRPError <<< show)) <$> (SRP.prepareV conf (toArrayBuffer oldRecord.s) newP)
  oldMasterPassword <- ExceptT $ Right <$> KI.importKey raw oldP (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  newMasterPassword <- ExceptT $ Right <$> KI.importKey raw newP (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  masterKeyDecryptedContent <- ExceptT $ (bimap (ProtocolError <<< CryptoError <<< show) IndexReference) <$> decryptJson oldMasterPassword (toArrayBuffer oldRecord.masterKeyContent)
  masterKeyEncryptedContent <- ExceptT $ (Right <<< fromArrayBuffer) <$> encryptJson newMasterPassword masterKeyDecryptedContent
  let newUserCard = UserCard $ oldRecord { c = fromArrayBuffer newC, v = newV, masterKeyContent = masterKeyEncryptedContent }
  let modifyData = { c: oldRecord.c, oldUserCard, newUserCard }
  let url = joinWith "/" [ "users", show oldRecord.c ]
  let body = (json $ encodeJson modifyData) :: RequestBody
  response :: AXW.Response String <- manageGenericRequest url PUT (Just body) RF.string
  except $ if isStatusCodeOk response.status
           then Right unit
           else Left (ProtocolError (ResponseError (unwrap response.status)))
