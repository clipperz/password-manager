
module Functions.Communication.Login where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), except, throwError, withExceptT)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt)
import Data.Codec.Argonaut (decode, encode)
import Data.Codec.Argonaut.Record as CAR
import Data.Either (note)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer, fromBigInt, hexStringCodec, toArrayBuffer, toBigInt)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.AppState (ProxyResponse(..))
import DataModel.Communication.Login (LoginStep1Response, LoginStep2Response, loginStep1ResponseCodec, loginStep2ResponseCodec)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRPVersions.SRP (SRPConf)
import DataModel.UserVersions.User (MasterKey)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.ArrayBuffer (arrayBufferToBigInt)
import Functions.Communication.Backend (ConnectionState, isStatusCodeOk, loginRequest)
import Functions.SRP as SRP
    
-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-UserSession-ID"

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type PrepareLoginResult = {
  c :: HexString
, p :: HexString
}

prepareLogin :: SRPConf -> Credentials -> ExceptT AppError Aff (PrepareLoginResult)
prepareLogin srpConf { username, password } = do
  c         <- liftAff $ fromArrayBuffer <$> SRP.prepareC srpConf username password
  p         <- liftAff $ fromArrayBuffer <$> SRP.prepareP srpConf username password

  pure {c, p}
  
-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LoginStep1Result = { aa :: BigInt
                        , a  :: BigInt
                        , s  :: HexString
                        , bb :: BigInt
                        }

loginStep1 :: ConnectionState -> HexString -> ExceptT AppError Aff (ProxyResponse LoginStep1Result)
loginStep1 connectionState@{srpConf} c = do
  (Tuple a aa) <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
  let url  = joinWith "/" ["login", "step1", show c] :: String
  let body = json $ encode (CAR.object "loginStep1Request" {c: hexStringCodec, aa: hexStringCodec}) { c, aa: fromBigInt aa }  :: RequestBody
  ProxyResponse newProxy step1Response <- loginRequest connectionState url POST (Just body) RF.json
  responseBody :: LoginStep1Response <- if isStatusCodeOk step1Response.status
                                          then except     $ (decode loginStep1ResponseCodec step1Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err) 
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
                        , masterKey :: MasterKey
                        }

loginStep2 :: ConnectionState -> HexString -> HexString -> LogintStep2Data -> ExceptT AppError Aff (ProxyResponse LoginStep2Result)
loginStep2 connectionState@{srpConf} c p { aa, bb, a, s } = do
  x  :: BigInt      <-  ExceptT $ (srpConf.kdf srpConf.hash (toArrayBuffer s) (toArrayBuffer p)) <#> (\ab -> note (ProtocolError $ SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab))
  ss :: BigInt      <- (ExceptT $ SRP.prepareSClient srpConf aa bb x a) # withExceptT (\err -> ProtocolError $ SRPError $ show err)
  kk :: ArrayBuffer <-  liftAff $ SRP.prepareK  srpConf ss
  m1 :: ArrayBuffer <-  liftAff $ SRP.prepareM1 srpConf c s aa bb kk
  let url  = joinWith "/" ["login", "step2", show c]      :: String
  let body = json $ encode (CAR.object "loginStep2Request" {m1: hexStringCodec}) { m1: fromArrayBuffer m1 } :: RequestBody
  ProxyResponse newProxy step2Response <- loginRequest connectionState url POST (Just body) RF.json
  responseBody :: LoginStep2Response   <- if isStatusCodeOk step2Response.status
                                          then except $     (decode loginStep2ResponseCodec step2Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err)
                                          else throwError $  ProtocolError $ ResponseError (unwrap step2Response.status)
  pure $ ProxyResponse newProxy { m1, kk, m2: responseBody.m2, masterKey: responseBody.masterKey }

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
