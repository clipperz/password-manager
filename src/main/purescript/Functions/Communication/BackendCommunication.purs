module Functions.Communication.BackendCommunication where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader as RE
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Control.Applicative (pure)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&))
import Data.HTTP.Method (Method)
import Data.Maybe (Maybe)
import Data.Ord((<=), (>=))
import Data.String.Common (joinWith)
import DataModel.Proxy (Proxy(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff)

-- ----------------------------------------------------------------------------

type Url = String

-- ----------------------------------------------------------------------------

doGenericRequest' :: forall a. Url 
                 -> Method 
                 -> Array RE.RequestHeader 
                 -> Maybe RequestBody 
                 -> RF.ResponseFormat a 
                 -> Aff (Either ProtocolError (AXW.Response a))
doGenericRequest' url method headers body resFormat = do
  lmap (\e -> RequestError e) <$> AXW.request (
    AXW.defaultRequest {
      url            = url
    , method         = Left method
    , headers        = headers
    , content        = body 
    , responseFormat = resFormat
    })


doGenericRequest :: forall a. Proxy -> RequestInfo a -> Aff (Either ProtocolError (AXW.Response a))
doGenericRequest (OnlineProxy baseUrl) (OnlineRequestInfo { url, method, headers, body, response }) =
  lmap (\e -> RequestError e) <$> AXW.request (
    AXW.defaultRequest {
        url            = joinWith "/" [baseUrl, url]
      , method         = Left method
      , headers        = headers
      , content        = body 
      , responseFormat = response
    }
  )
doGenericRequest  OfflineProxy   (OfflineRequestInfo { url: _, method: _, body: _, response: _ }) =
  pure $ Left $ ResponseError 500 -- TODO
doGenericRequest (OnlineProxy _) (OfflineRequestInfo _) = pure $ Left $ IllegalRequest "Cannot do an offline request with an online proxy"
doGenericRequest  OfflineProxy   (OnlineRequestInfo  _) = pure $ Left $ IllegalRequest "Cannot do an online request with an offline proxy"

data RequestInfo a = OnlineRequestInfo  { url :: Url 
                                        , method :: Method
                                        , headers :: Array RE.RequestHeader
                                        , body :: Maybe RequestBody
                                        , response :: RF.ResponseFormat a
                                        } 
                   | OfflineRequestInfo { url :: Url 
                                        , method :: Method
                                        , body :: Maybe RequestBody
                                        , response :: RF.ResponseFormat a
                                        }

isStatusCodeOk :: StatusCode -> Boolean
isStatusCodeOk code = (code >= (StatusCode 200)) && (code <= (StatusCode 299))
