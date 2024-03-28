module Functions.User where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), throwError)
import Control.Semigroupoid ((<<<))
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (encode)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), HexString, fromArrayBuffer, toArrayBuffer, toString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.AppState (ProxyResponse(..), AppState, InvalidStateError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRPVersions.CurrentSRPVersions (currentSRPVersion)
import DataModel.UserVersions.CurrentUserVersions (currentMasterKeyEncodingVersion)
import DataModel.UserVersions.User (MasterKey, RequestUserCard(..), requestUserCardCodec)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Communication.Backend (isStatusCodeOk, genericRequest)
import Functions.EncodeDecode (decryptArrayBuffer, encryptArrayBuffer, importCryptoKeyAesGCM)
import Functions.SRP as SRP

type ModifyUserData = { c :: HexString
                      , p :: HexString
                      , s :: HexString
                      , masterKey :: MasterKey
                      }

changeUserPassword :: AppState -> String -> ExceptT AppError Aff (ProxyResponse ModifyUserData)
changeUserPassword {srpConf, c: Just oldC, p: Just oldP, username: Just username, proxy, hash: hashFunc, masterKey: Just (Tuple masterKeyContent _)} newPassword = do
  s    <- liftAff $ SRP.randomArrayBuffer 32
  newC <- liftAff $ SRP.prepareC srpConf username newPassword
  newP <- liftAff $ SRP.prepareP srpConf username newPassword
  newV <- ExceptT $ (lmap (ProtocolError <<< SRPError <<< show)) <$> (SRP.prepareV srpConf s newP)
  
  oldMasterPassword         <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer oldP)
  masterKeyDecryptedContent <- ExceptT $ decryptArrayBuffer     oldMasterPassword (toArrayBuffer $ masterKeyContent) <#> (lmap (ProtocolError <<< CryptoError <<< show))
  
  newMasterPassword         <- liftAff $ importCryptoKeyAesGCM  newP
  masterKeyEncryptedContent <- liftAff $ encryptArrayBuffer     newMasterPassword  masterKeyDecryptedContent         <#> fromArrayBuffer
  
  masterKey                 <- pure    $ Tuple masterKeyEncryptedContent currentMasterKeyEncodingVersion
  
  let newUserCard = RequestUserCard { c: fromArrayBuffer newC
                                    , v: newV
                                    , s: fromArrayBuffer s
                                    , masterKey
                                    , srpVersion: currentSRPVersion
                                    , originMasterKey: Just $ masterKeyContent
                                    }
  let url         = joinWith "/" [ "users", toString Hex oldC ]
  let body        = (json $ encode requestUserCardCodec newUserCard) :: RequestBody
  
  ProxyResponse proxy' response <- genericRequest {hashFunc, proxy, srpConf, c: fromArrayBuffer newC, p: fromArrayBuffer newP} url PUT (Just body) RF.ignore
  if isStatusCodeOk response.status
    then pure       $ ProxyResponse proxy' { c: fromArrayBuffer newC
                                           , p: fromArrayBuffer newP
                                           , s: fromArrayBuffer s
                                           , masterKey
                                           }
    else throwError $ ProtocolError (ResponseError (unwrap response.status))
changeUserPassword _ _ = throwError $ InvalidStateError (CorruptedState "changePassword")
