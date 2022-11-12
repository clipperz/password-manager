module Functions.Communication.Logout where

import Affjax.ResponseFormat as RF
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either)
import Data.Function (($))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit)
import DataModel.AppState (AppError)
import Effect.Aff (Aff)
import Functions.Communication.BackendCommunication (manageGenericRequest)
import Functions.State (resetState)

doLogout :: Boolean -> Aff (Either AppError Unit)
doLogout isLock = runExceptT $ do
  _ <- manageGenericRequest "logout" POST Nothing RF.string
  resetState -- TODO: dependes on lock behaviour and on isLock value

