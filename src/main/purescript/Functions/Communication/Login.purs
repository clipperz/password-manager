
module Functions.Communication.Login where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), except, throwError, withExceptT)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt)
import Data.Either (note)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer, fromBigInt, toArrayBuffer, toBigInt)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.Communication.Login (LoginStep1Response, LoginStep2Response)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (SRPConf, HashFunction)
import DataModel.AppState (Proxy(..), ProxyResponse(..))
import DataModel.User (MasterKey, UserInfoReferences)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.ArrayBuffer (arrayBufferToBigInt)
import Functions.Communication.Backend (isStatusCodeOk, manageGenericRequest)
import Functions.SRP as SRP
import Functions.User (decryptUserInfoReferences)
    
-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-UserSession-ID"

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type PrepareLoginResult = {
  c :: HexString
, p :: HexString
}

prepareLogin :: Proxy -> SRPConf -> Credentials -> ExceptT AppError Aff (ProxyResponse PrepareLoginResult)
prepareLogin       (StaticProxy _)     _       _                      = throwError $ UnhandledCondition "TODO [implement offline login]"
prepareLogin proxy@(OnlineProxy _ _ _) srpConf { username, password } = do
  c         <- liftAff $ fromArrayBuffer <$> SRP.prepareC srpConf username password
  p         <- liftAff $ fromArrayBuffer <$> SRP.prepareP srpConf username password

  pure $ ProxyResponse proxy {c, p}
  
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LoginStep1Result = { aa :: BigInt
                        , a  :: BigInt
                        , s  :: HexString
                        , bb :: BigInt
                        }

loginStep1 :: Proxy -> HashFunction -> SRPConf -> HexString -> ExceptT AppError Aff (ProxyResponse LoginStep1Result)
loginStep1 proxy hashFunc srpConf c = do
  (Tuple a aa) <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
  let url  = joinWith "/" ["login", "step1", show c] :: String
  let body = json $ encodeJson { c, aa: fromBigInt aa }  :: RequestBody
  ProxyResponse newProxy step1Response <- manageGenericRequest {hashFunc, proxy} url POST (Just body) RF.json
  responseBody :: LoginStep1Response <- if isStatusCodeOk step1Response.status
                                          then except     $ (decodeJson step1Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err) 
                                          else throwError $  ProtocolError (ResponseError (unwrap step1Response.status))
  bb :: BigInt <- except $ (toBigInt responseBody.bb) # note (ProtocolError $ SRPError "Error in converting B from String to BigInt")
  if bb == fromInt (0)
    then throwError $ ProtocolError (SRPError "Server returned B == 0")
    else pure $ ProxyResponse newProxy { aa, a, s: responseBody.s, bb }

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LogintStep2Data = { aa :: BigInt
                       , bb :: BigInt
                       , a  :: BigInt
                       , s  :: HexString
                       }

type LoginStep2Result = { m1 :: ArrayBuffer
                        , kk :: ArrayBuffer
                        , m2 :: HexString
                        , userInfoReferences :: UserInfoReferences
                        , masterKey :: MasterKey
                        }

loginStep2 :: Proxy -> HashFunction -> SRPConf -> HexString -> HexString -> LogintStep2Data -> ExceptT AppError Aff (ProxyResponse LoginStep2Result)
loginStep2 proxy hashFunc srpConf c p { aa, bb, a, s } = do
  x  :: BigInt      <-  ExceptT $ (srpConf.kdf srpConf.hash (toArrayBuffer s) (toArrayBuffer p)) <#> (\ab -> note (ProtocolError $ SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab))
  ss :: BigInt      <- (ExceptT $ SRP.prepareSClient srpConf aa bb x a) # withExceptT (\err -> ProtocolError $ SRPError $ show err)
  kk :: ArrayBuffer <-  liftAff $ SRP.prepareK  srpConf ss
  m1 :: ArrayBuffer <-  liftAff $ SRP.prepareM1 srpConf c s aa bb kk
  let url  = joinWith "/" ["login", "step2", show c]      :: String
  let body = json $ encodeJson { m1: fromArrayBuffer m1 } :: RequestBody
  ProxyResponse newProxy step2Response <- manageGenericRequest { hashFunc, proxy } url POST (Just body) RF.json
  responseBody :: LoginStep2Response <- if isStatusCodeOk step2Response.status
                                          then except $     (decodeJson step2Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err)
                                          else throwError $  ProtocolError $ ResponseError (unwrap step2Response.status)
  userInfoReferences <- decryptUserInfoReferences responseBody.masterKey p
  pure $ ProxyResponse newProxy { m1, kk, m2: responseBody.m2, userInfoReferences, masterKey: responseBody.masterKey }

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LoginStateUpdate = {
  userInfoReferences :: Maybe UserInfoReferences
, masterKey          :: Maybe MasterKey
, username           :: Maybe String
, password           :: Maybe String
, s                  :: Maybe HexString
, c                  :: Maybe HexString
, p                  :: Maybe HexString
}
