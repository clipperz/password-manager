module Functions.Communication.Signup where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Affjax.Web as AXW
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Function (flip, ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, hex, fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.User (RequestUserCard(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.JSState (saveAppState, getAppState)
import Functions.SRP as SRP
import Functions.Signup (prepareSignupParameters)

signupUser :: Credentials -> ExceptT AppError Aff HexString
signupUser credentials = do
  currentState                      <- ExceptT $ liftEffect $ getAppState
  request@{user: RequestUserCard u} <- flip withExceptT (ExceptT (prepareSignupParameters credentials)) (show >>> SRPError >>> ProtocolError)
  let url  = joinWith "/" ["users", show u.c]
  let body = (json $ encodeJson request) :: RequestBody
  --- --------------------------- 
  sessionKey <- liftAff (fromArrayBuffer <$> SRP.randomArrayBuffer 32) --- TODO: maybe to manage with session middleware
  liftAff (liftEffect $ saveAppState (currentState { sessionKey = Just sessionKey }))
  --- --------------------------- 
  response :: AXW.Response String <- manageGenericRequest url POST (Just body) RF.string
  if isStatusCodeOk response.status
    then pure $ hex response.body
    else throwError $ ProtocolError (ResponseError (unwrap response.status))

