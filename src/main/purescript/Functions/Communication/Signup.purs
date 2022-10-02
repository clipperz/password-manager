module Functions.Communication.Signup where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except)
import Control.Semigroupoid ((>>>), (<<<))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, hex, fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.AppState (AppError(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.JSState (modifyAppState, getAppState)
import Functions.Signup (prepareSignupParameters)
import Functions.SRP as SRP

signupUser :: SRP.SRPConf -> Credentials -> ExceptT AppError Aff HexString
signupUser conf credentials = do
  currentState <- ExceptT $ liftEffect $ getAppState
  request <- ExceptT $ (lmap (ProtocolError <<< SRPError <<< show)) <$> prepareSignupParameters conf credentials
  let url = joinWith "/" ["users", show request.user.c]
  let body = (json $ encodeJson request) :: RequestBody
  --- --------------------------- 
  sessionKey :: HexString <- ExceptT $ (lmap ProtocolError) <$> ((fromArrayBuffer >>> Right) <$> SRP.randomArrayBuffer 32) --- TODO: maybe to manage with session middleware
  ExceptT $ Right <$> modifyAppState (currentState { sessionKey = Just sessionKey })
  --- --------------------------- 
  response :: AXW.Response String <- manageGenericRequest url PUT (Just body) RF.string
  except $ if isStatusCodeOk response.status
           then Right $ hex response.body
           else Left (ProtocolError (ResponseError (unwrap response.status)))

