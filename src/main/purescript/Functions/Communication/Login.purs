
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
import DataModel.AppState (AppState, AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (IndexReference)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (manageGenericRequest, manageGenericRequest, isStatusCodeOk)
import Functions.ArrayBuffer (arrayBufferToBigInt)
import Functions.JSState (updateAppState, getAppState)
import Functions.SRP as SRP

import Effect.Class.Console (log)

    
-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-UserSession-ID"

login :: SRP.SRPConf -> ExceptT AppError Aff IndexReference
login srpConf = do
  currentState <- ExceptT $ liftEffect $ getAppState
  if isJust currentState.sessionKey
    then ExceptT $ Right <$> updateAppState currentState
    else do
      sessionKey :: HexString   <- ExceptT $ (fromArrayBuffer >>> Right) <$> SRP.randomArrayBuffer 32
      ExceptT $ Right <$> updateAppState (currentState { sessionKey = Just sessionKey })
  loginStep1Result <- loginStep1 srpConf
  { m1, kk, m2, encIndexReference: indexReference } <- loginStep2 srpConf loginStep1Result
  check :: Boolean <- ExceptT $ Right <$> SRP.checkM2 SRP.baseConfiguration loginStep1Result.aa m1 kk (toArrayBuffer m2)
  case check of
    true  -> except $ Right indexReference
    false -> except $ Left (ProtocolError $ SRPError "Client M2 doesn't match with server M2")

-- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type LoginStep1Response = { s  :: HexString
                          , bb :: HexString
                          }

type LoginStep1Result = { aa :: BigInt
                        , a  :: BigInt
                        , s  :: HexString
                        , bb :: BigInt
                        }

loginStep1 :: SRP.SRPConf -> ExceptT AppError Aff LoginStep1Result
loginStep1 srpConf = do
  { proxy: _, c: mc, p: _, sessionKey: _, toll: _ } <- ExceptT $ liftEffect $ getAppState
  c <- except $ note (InvalidStateError "c is Nothing") mc
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

type LoginStep2Response = { m2 :: HexString
                          , encIndexReference :: HexString
                          }

type LoginStep2Result = { m1 :: ArrayBuffer
                        , kk :: ArrayBuffer
                        , m2 :: HexString
                        , encIndexReference :: HexString
                        }

loginStep2 :: SRP.SRPConf -> LogintStep2Data -> ExceptT AppError Aff LoginStep2Result
loginStep2 srpConf { aa, bb, a, s } = do
  { proxy: _, c: mc, p: mp, sessionKey: _, toll: _ } <- ExceptT $ liftEffect $ getAppState
  c <- except $ note (InvalidStateError "c is Nothing") mc
  p <- except $ note (InvalidStateError "p is Nothing") mp
  x  :: BigInt      <- ExceptT $ (\ab -> note (ProtocolError $ SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab)) <$> (srpConf.kdf (toArrayBuffer s) (toArrayBuffer p))
  ss :: BigInt      <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareSClient srpConf aa bb x a)
  kk :: ArrayBuffer <- ExceptT $ Right <$> (SRP.prepareK srpConf ss)
  m1 :: ArrayBuffer <- ExceptT $ Right <$> (SRP.prepareM1 srpConf c s aa bb kk)
  let url  = joinWith "/" ["login", "step2", show c] :: String
  let body = json $ encodeJson { m1: fromArrayBuffer m1 }  :: RequestBody
  step2Response <- manageGenericRequest url POST (Just body) RF.json
  responseBody :: LoginStep2Response <- except $ if isStatusCodeOk step2Response.status
                                                 then lmap (\err -> ProtocolError $ DecodeError $ show err) (decodeJson step2Response.body)
                                                 else Left (ProtocolError $ ResponseError (unwrap step2Response.status))
  except $ Right { m1, kk, m2: responseBody.m2, encIndexReference: responseBody.encIndexReference }
