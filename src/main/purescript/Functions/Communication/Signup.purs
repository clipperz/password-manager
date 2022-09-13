module Functions.Communication.Signup where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.State (StateT)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Either (Either(..))
import Data.Function (($))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, hex)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple)
import DataModel.AppState (AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import Functions.Communication.BackendCommunication (baseUrl, isStatusCodeOk, doGenericRequest)
import Functions.State (makeStateT)

type UserCard = {
    c :: HexString
  , v :: HexString
  , s :: HexString
  , srpVersion :: String
  , masterKeyEncodingVersion :: String
  , masterKeyContent :: HexString
}
type RegisterUserRequest = {
    user :: UserCard
  , indexCardReference :: HexString
  , indexCardContent   :: HexString
  , cards :: Array (Tuple HexString HexString)
}

registerUser :: RegisterUserRequest -> Aff (Either ProtocolError HexString)
-- registerUser :: RegisterUserRequest -> StateT AppState Aff (Either ProtocolError HexString)
registerUser request = do
  let url = joinWith "/" [baseUrl, "users", show request.user.c]
  let body = (json $ encodeJson request) :: RequestBody
  registerUserResponse <- doGenericRequest url PUT [] (Just body) RF.string 
  pure $ case registerUserResponse of
  -- makeStateT $ pure $ case registerUserResponse of
    Left  error    -> Left error
    Right response -> if isStatusCodeOk response.status
                      then Right $ hex response.body
                      else Left  (ResponseError (unwrap response.status))
