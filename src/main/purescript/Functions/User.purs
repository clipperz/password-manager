module Functions.User where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Control.Semigroupoid ((<<<))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (decrypt, encrypt, raw, unwrapKey, CryptoKey)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (encode)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, fromArrayBuffer, toArrayBuffer, toString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.AppState (ProxyResponse(..), AppState, InvalidStateError(..))
import DataModel.Codec as Codec
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRPCodec as SRPCodec
import DataModel.User (MasterKey, MasterKeyEncodingVersion(..), RequestUserCard(..), SRPVersion(..), UserInfoReferences)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception as EX
import Functions.Communication.Backend (isStatusCodeOk, genericRequest)
import Functions.EncodeDecode (decryptJson, encryptJson)
import Functions.SRP as SRP

type ModifyUserData = { c :: HexString
                      , p :: HexString
                      , s :: HexString
                      }

changeUserPassword :: AppState -> String -> ExceptT AppError Aff (ProxyResponse ModifyUserData)
changeUserPassword {srpConf, c: Just oldC, p: Just oldP, username: Just username, proxy, hash: hashFunc, masterKey: Just (Tuple masterKeyContent _)} newPassword = do
  s        <- liftAff $ SRP.randomArrayBuffer 32
  newC     <- liftAff $ SRP.prepareC srpConf username newPassword
  newP     <- liftAff $ SRP.prepareP srpConf username newPassword
  newV <- ExceptT $ (lmap (ProtocolError <<< SRPError <<< show)) <$> (SRP.prepareV srpConf s newP)
  oldMasterPassword <- liftAff $ KI.importKey raw (toArrayBuffer oldP) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  newMasterPassword <- liftAff $ KI.importKey raw newP (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  masterKeyDecryptedContent <- ExceptT $ (lmap (ProtocolError <<< CryptoError <<< show)) <$> decryptJson Codec.userInfoReferencesCodec oldMasterPassword (toArrayBuffer $ masterKeyContent)
  masterKeyEncryptedContent <- liftAff $ fromArrayBuffer <$> encryptJson Codec.userInfoReferencesCodec newMasterPassword masterKeyDecryptedContent
  let newUserCard = RequestUserCard { c: fromArrayBuffer newC
                                    , v: newV
                                    , s: fromArrayBuffer s
                                    , masterKey: Tuple masterKeyEncryptedContent V_1
                                    , srpVersion: V_6a
                                    , originMasterKey: Just $ masterKeyContent
                                    }
  let url         = joinWith "/" [ "users", toString Hex oldC ]
  let body        = (json $ encode SRPCodec.requestUserCardCodec newUserCard) :: RequestBody
  
  ProxyResponse proxy' response <- genericRequest {hashFunc, proxy, srpConf, c: fromArrayBuffer newC, p: fromArrayBuffer newP} url PUT (Just body) RF.ignore
  if isStatusCodeOk response.status
    then pure $ ProxyResponse proxy' { c: fromArrayBuffer newC
                                     , p: fromArrayBuffer newP
                                     , s: fromArrayBuffer s
                                     }
    else throwError $ ProtocolError (ResponseError (unwrap response.status))
changeUserPassword _ _ = throwError $ InvalidStateError (CorruptedState "State is corrupted")

decryptUserInfoReferences :: MasterKey -> HexString -> ExceptT AppError Aff UserInfoReferences
decryptUserInfoReferences (Tuple encryptedRef _) p = do -- TODO: handle version
  masterPassword :: CryptoKey <- liftAff $ KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  mapCryptoError $ ExceptT $ decryptJson Codec.userInfoReferencesCodec masterPassword (toArrayBuffer encryptedRef)

  where 
    mapCryptoError :: forall a. ExceptT EX.Error Aff a -> ExceptT AppError Aff a
    mapCryptoError = withExceptT (\e -> ProtocolError $ CryptoError $ "Decrypt UserInfoReferences: " <> EX.message e)
