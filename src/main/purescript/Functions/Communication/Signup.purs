module Functions.Communication.Signup where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Function (flip, ($))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, hex)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import DataModel.AppError (AppError(..))
import DataModel.AppState (Proxy(..), ProxyResponse(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (SRPConf, HashFunction)
import DataModel.User (RequestUserCard(..))
import Effect.Aff (Aff)
import Functions.Communication.Backend (isStatusCodeOk, signupRequest)
import Functions.Communication.Login (PrepareLoginResult)
import Functions.Signup (prepareSignupParameters)

type SessionKey = HexString

type SignupResult = PrepareLoginResult

signupUser :: Proxy -> HashFunction -> SRPConf -> Credentials -> ExceptT AppError Aff (ProxyResponse SignupResult)
signupUser       (StaticProxy _)     _        _       _           = throwError $ InvalidOperationError "Cannot register new user in static usage"
signupUser proxy@(OnlineProxy _ _ _) hashFunc srpConf credentials = do
  request@{user: RequestUserCard u, p} <- flip withExceptT (ExceptT (prepareSignupParameters srpConf credentials)) (show >>> SRPError >>> ProtocolError)
  let path  = joinWith "/" ["users", show u.c]
  let body = (json $ encodeJson request) :: RequestBody
  --- ---------------------------
  ProxyResponse newProxy response <- signupRequest {proxy, hashFunc, srpConf, c: hex "", p: hex ""} path POST (Just body) RF.string
  if isStatusCodeOk response.status
    then pure $ ProxyResponse newProxy {c: u.c, p: p}
    else throwError $ ProtocolError (ResponseError (unwrap response.status))

