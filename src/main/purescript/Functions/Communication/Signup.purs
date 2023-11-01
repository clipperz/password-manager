module Functions.Communication.Signup where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Control.Semigroupoid ((>>>))
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Function (flip, ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (SRPConf, HashFunction)
import DataModel.StatelessAppState (Proxy, updateSessionKey)
import DataModel.User (RequestUserCard(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Communication.StatelessBackend (isStatusCodeOk, manageGenericRequest)
import Functions.SRP as SRP
import Functions.Signup (prepareSignupParameters)

type SessionKey = HexString

signupUser :: Proxy -> HashFunction -> SRPConf -> Credentials -> ExceptT AppError Aff Proxy
signupUser proxy hashFunc srpConf credentials = do
  request@{user: RequestUserCard u} <- flip withExceptT (ExceptT (prepareSignupParameters srpConf credentials)) (show >>> SRPError >>> ProtocolError)
  let url  = joinWith "/" ["users", show u.c]
  let body = (json $ encodeJson request) :: RequestBody
  --- ---------------------------
  sessionKey <- liftAff (fromArrayBuffer <$> SRP.randomArrayBuffer 32) --- NOTE: maybe to manage with session middleware
  let connectionState = {proxy: updateSessionKey proxy (Just sessionKey), hashFunc}
  --- ---------------------------
  Tuple updatedProxy response <- manageGenericRequest (connectionState) url POST (Just body) RF.string
  if isStatusCodeOk response.status
    then pure $ updatedProxy
    else throwError $ ProtocolError (ResponseError (unwrap response.status))

