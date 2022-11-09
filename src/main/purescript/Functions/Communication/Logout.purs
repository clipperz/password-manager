module Functions.Communication.Logout where


import Affjax.RequestBody (RequestBody, json)
-- import Affjax.RequestHeader as RE
import Affjax.ResponseFormat as RF
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Control.Semigroupoid ((>>>))
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.BigInt (BigInt, fromInt)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Communication.Login (LoginStep1Response, LoginStep2Response)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.User (IndexReference)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.BackendCommunication (manageGenericRequest, isStatusCodeOk)
import Functions.State (resetState)

doLogout :: Boolean -> Aff (Either AppError Unit)
doLogout isLock = runExceptT $ do
  _ <- manageGenericRequest "logout" POST Nothing RF.string
  resetState -- TODO: dependes on lock behaviour and on isLock value

