
module Functions.Communication.Login where

import Affjax.RequestBody (RequestBody, json)
-- import Affjax.RequestHeader as RE
import Affjax.ResponseFormat as RF
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.BigInt (BigInt, fromInt)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, toBigInt, fromBigInt, toArrayBuffer, fromArrayBuffer)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.Login (LoginStep1Response, LoginStep2Response)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.User (IndexReference, UserInfoReferences)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (manageGenericRequest, isStatusCodeOk)
import Functions.ArrayBuffer (arrayBufferToBigInt)
import Functions.JSState (modifyAppState, getAppState, updateAppState)
import Functions.State (getSRPConf, getSRPConfFromState)
import Functions.SRP as SRP
import Functions.User (decryptUserInfoReferences)
    
-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-UserSession-ID"

login :: ExceptT AppError Aff Unit
login = do
  currentState <- ExceptT $ liftEffect $ getAppState
  let srpConf = getSRPConfFromState currentState
  if isJust currentState.sessionKey
    then ExceptT $ Right <$> modifyAppState currentState
    else do
      sessionKey :: HexString   <- ExceptT $ (fromArrayBuffer >>> Right) <$> SRP.randomArrayBuffer 32
      ExceptT $ Right <$> modifyAppState (currentState { sessionKey = Just sessionKey })
  loginStep1Result <- loginStep1
  { m1, kk, m2, userInfoReferences } <- loginStep2 loginStep1Result
  ExceptT $ updateAppState { userInfoReferences: Just userInfoReferences }
  check :: Boolean <- ExceptT $ Right <$> SRP.checkM2 srpConf loginStep1Result.aa m1 kk (toArrayBuffer m2)
  case check of
    true  -> except $ Right unit
    false -> except $ Left (ProtocolError $ SRPError "Client M2 doesn't match with server M2")

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LoginStep1Result = { aa :: BigInt
                        , a  :: BigInt
                        , s  :: HexString
                        , bb :: BigInt
                        }

loginStep1 :: ExceptT AppError Aff LoginStep1Result
loginStep1 = do
  srpConf <- ExceptT $ liftEffect getSRPConf
  { proxy: _, c: mc, p: _, sessionKey: _, toll: _ } <- ExceptT $ liftEffect $ getAppState
  c <- except $ note (InvalidStateError (MissingValue "Missing c")) mc
  (Tuple a aa) <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
  let url  = joinWith "/" ["login", "step1", show c] :: String
  let body = json $ encodeJson { c, aa: fromBigInt aa }  :: RequestBody
  step1Response <- manageGenericRequest url POST (Just body) RF.json
  responseBody :: LoginStep1Response <- except $ if isStatusCodeOk step1Response.status
                                                 then lmap (\err -> ProtocolError $ DecodeError $ show err) (decodeJson step1Response.body)
                                                 else Left (ProtocolError $ ResponseError (unwrap step1Response.status))
  bb :: BigInt <- except $ note (ProtocolError $ SRPError "Error in converting B from String to BigInt") (toBigInt responseBody.bb)
  except $ if bb == fromInt (0)
           then Left $ ProtocolError $ SRPError "Server returned B == 0"
           else Right { aa, a, s: responseBody.s, bb }

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

loginStep2 :: LogintStep2Data -> ExceptT AppError Aff LoginStep2Result
loginStep2 { aa, bb, a, s } = do
  srpConf <- ExceptT $ liftEffect getSRPConf
  { proxy: _, c: mc, p: mp, sessionKey: _, toll: _ } <- ExceptT $ liftEffect $ getAppState
  c <- except $ note (InvalidStateError (MissingValue "Missing c")) mc
  p <- except $ note (InvalidStateError (MissingValue "Missing p")) mp
  x  :: BigInt      <- ExceptT $ (\ab -> note (ProtocolError $ SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab)) <$> (srpConf.kdf srpConf.hash (toArrayBuffer s) (toArrayBuffer p))
  ss :: BigInt      <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareSClient srpConf aa bb x a)
  kk :: ArrayBuffer <- ExceptT $ Right <$> (SRP.prepareK srpConf ss)
  m1 :: ArrayBuffer <- ExceptT $ Right <$> (SRP.prepareM1 srpConf c s aa bb kk)
  let url  = joinWith "/" ["login", "step2", show c] :: String
  let body = json $ encodeJson { m1: fromArrayBuffer m1 }  :: RequestBody
  step2Response <- manageGenericRequest url POST (Just body) RF.json
  responseBody :: LoginStep2Response <- except $ if isStatusCodeOk step2Response.status
                                                 then lmap (\err -> ProtocolError $ DecodeError $ show err) (decodeJson step2Response.body)
                                                 else Left (ProtocolError $ ResponseError (unwrap step2Response.status))
  userInfoReferences <- decryptUserInfoReferences responseBody.encUserInfoReferences
  except $ Right { m1, kk, m2: responseBody.m2, userInfoReferences }
