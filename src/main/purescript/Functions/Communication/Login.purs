
module Functions.Communication.Login where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind, discard)
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
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..))
import DataModel.Communication.Login (LoginStep1Response, LoginStep2Response)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRP (SRPConf)
import DataModel.User (UserInfoReferences)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.ArrayBuffer (arrayBufferToBigInt)
import Functions.Communication.BackendCommunication (manageGenericRequest, isStatusCodeOk)
import Functions.JSState (saveAppState, getAppState, updateAppState)
import Functions.SRP as SRP
import Functions.User (decryptUserInfoReferences)
    
-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-UserSession-ID"

login :: SRPConf -> HexString -> HexString -> ExceptT AppError Aff Unit
login srpConf c p = do
  currentState <- ExceptT $ liftEffect $ getAppState
  if isJust currentState.sessionKey
    then liftAff $ liftEffect $ saveAppState currentState
    else do
      sessionKey :: HexString <- liftAff $ fromArrayBuffer <$> SRP.randomArrayBuffer 32
      liftAff $ (liftEffect $ saveAppState (currentState { sessionKey = Just sessionKey }))
  loginStep1Result@{ s } <- loginStep1 srpConf c
  { m1, kk, m2, userInfoReferences } <- loginStep2 srpConf c p loginStep1Result
  ExceptT $ updateAppState { s: Just s, userInfoReferences: Just userInfoReferences }
  check :: Boolean <- liftAff $ SRP.checkM2 srpConf loginStep1Result.aa m1 kk (toArrayBuffer m2)
  case check of
    true  -> pure unit
    false -> throwError $ ProtocolError (SRPError "Client M2 doesn't match with server M2")

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LoginStep1Result = { aa :: BigInt
                        , a  :: BigInt
                        , s  :: HexString
                        , bb :: BigInt
                        }

loginStep1 :: SRPConf -> HexString -> ExceptT AppError Aff LoginStep1Result
loginStep1 srpConf c = do
  (Tuple a aa) <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
  let url  = joinWith "/" ["login", "step1", show c] :: String
  let body = json $ encodeJson { c, aa: fromBigInt aa }  :: RequestBody
  step1Response <- manageGenericRequest url POST (Just body) RF.json
  responseBody :: LoginStep1Response <- if isStatusCodeOk step1Response.status
                                          then except     $ (decodeJson step1Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err) 
                                          else throwError $  ProtocolError (ResponseError (unwrap step1Response.status))
  bb :: BigInt <- except $ (toBigInt responseBody.bb) # note (ProtocolError $ SRPError "Error in converting B from String to BigInt")
  if bb == fromInt (0)
    then throwError $ ProtocolError (SRPError "Server returned B == 0")
    else pure { aa, a, s: responseBody.s, bb }

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
                        }

loginStep2 :: SRPConf -> HexString -> HexString -> LogintStep2Data -> ExceptT AppError Aff LoginStep2Result
loginStep2 srpConf c p { aa, bb, a, s } = do
  x  :: BigInt      <-  ExceptT $ (\ab -> note (ProtocolError $ SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab)) <$> (srpConf.kdf srpConf.hash (toArrayBuffer s) (toArrayBuffer p))
  ss :: BigInt      <- (ExceptT $ SRP.prepareSClient srpConf aa bb x a) # withExceptT (\err -> ProtocolError $ SRPError $ show err)
  kk :: ArrayBuffer <-  liftAff $ SRP.prepareK  srpConf ss
  m1 :: ArrayBuffer <-  liftAff $ SRP.prepareM1 srpConf c s aa bb kk
  let url  = joinWith "/" ["login", "step2", show c]      :: String
  let body = json $ encodeJson { m1: fromArrayBuffer m1 } :: RequestBody
  step2Response <- manageGenericRequest url POST (Just body) RF.json
  responseBody :: LoginStep2Response <- if isStatusCodeOk step2Response.status
                                          then except $     (decodeJson step2Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err)
                                          else throwError $  ProtocolError $ ResponseError (unwrap step2Response.status)
  userInfoReferences <- decryptUserInfoReferences responseBody.encUserInfoReferences p
  pure { m1, kk, m2: responseBody.m2, userInfoReferences }
