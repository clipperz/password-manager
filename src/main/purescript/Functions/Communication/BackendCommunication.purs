module Functions.Communication.BackendCommunication where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader as RE
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe)
import Data.Ord((<=), (>=))
import Data.Semigroup ((<>)) 
import Data.Show (class Show, show)
import Effect.Aff (Aff)

-- ----------------------------------------------------------------------------

type Url = String

baseUrl :: Url 
baseUrl = "http://localhost:8090" --TODO: get from configuration file/build

-- ----------------------------------------------------------------------------

data ProtocolError = RequestError AXW.Error | ResponseError Int | SRPError String | DecodeError String | CryptoError String
instance showProtobufRequestError :: Show ProtocolError where
  show (RequestError err) = "Request Error: "  <> AXW.printError err
  show (ResponseError i)  = "Response Error: " <> "response status code " <> show i
  show (SRPError err)     = "SRP Error: "      <> err
  show (DecodeError err)  = "Decode Error: "   <> err
  show (CryptoError err)  = "Decode Error: "   <> err

-- ----------------------------------------------------------------------------

doGenericRequest :: forall a. Url -> Method -> Array RE.RequestHeader -> Maybe RequestBody -> RF.ResponseFormat a -> Aff (Either ProtocolError (AXW.Response a))
doGenericRequest url method headers body resFormat =
  lmap (\e -> RequestError e) <$> AXW.request (
    AXW.defaultRequest {
      url            = url
    , method         = Left method
    , headers        = headers
    , content        = body 
    , responseFormat = resFormat
    }
  )

isStatusCodeOk :: StatusCode -> Boolean
isStatusCodeOk code = (code >= (StatusCode 200)) && (code <= (StatusCode 299))
