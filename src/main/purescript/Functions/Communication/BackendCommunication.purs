module Functions.Communication.BackendCommunication where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader as RE
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.State (StateT(..), get, modify_)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe(..))
import Data.HexString (hex)
import Data.Ord((<=), (>=))
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Show (class Show, show)
import DataModel.AppState (AppState)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)
import Functions.State (makeStateT)

-- ----------------------------------------------------------------------------

type Url = String

baseUrl :: Url 
baseUrl = "http://localhost:8090" --TODO: get from configuration file/build

-- ----------------------------------------------------------------------------

doGenericRequest :: forall a. Url 
                 -> Method 
                 -> Array RE.RequestHeader 
                 -> Maybe RequestBody 
                 -> RF.ResponseFormat a 
                 -> Aff (Either ProtocolError (AXW.Response a))
                --  -> StateT AppState Aff (Either ProtocolError (AXW.Response a))
doGenericRequest url method headers body resFormat = do
  -- result <- makeStateT (lmap (\e -> RequestError e) <$> AXW.request (
  lmap (\e -> RequestError e) <$> AXW.request (
    AXW.defaultRequest {
      url            = url
    , method         = Left method
    , headers        = headers
    , content        = body 
    , responseFormat = resFormat
    })
  -- )
  -- modify_ (\currentState -> currentState { toll = Just $ hex "0ef2" })
  -- pure $ result


isStatusCodeOk :: StatusCode -> Boolean
isStatusCodeOk code = (code >= (StatusCode 200)) && (code <= (StatusCode 299))
