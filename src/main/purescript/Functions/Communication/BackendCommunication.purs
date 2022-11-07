module Functions.Communication.BackendCommunication where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT, runExceptT)
import Data.Array (filter)
import Data.Bifunctor (lmap)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.HexString (hex)
import Data.HeytingAlgebra ((&&))
import Data.HTTP.Method (Method)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Ord((<=), (>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Data.Unit (unit)
import DataModel.AppState as AS
import DataModel.AsyncValue (AsyncValue(..), arrayFromAsyncValue, toLoading)
import DataModel.Proxy (Proxy(..))
import DataModel.SRP (hashFuncSHA256)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff (Aff, forkAff, delay)
import Effect.Class (liftEffect)
import Functions.HashCash (TollChallenge, computeReceipt)
import Functions.JSState (getAppState, modifyAppState, updateAppState)
import Functions.State (getHashFunctionFromAppState)

-- ----------------------------------------------------------------------------

type Url = String

-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-usersession-id"

tollHeaderName :: String
tollHeaderName = "clipperz-hashcash-tollchallenge"

tollCostHeaderName :: String
tollCostHeaderName = "clipperz-hashcash-tollcost"

tollReceiptHeaderName :: String
tollReceiptHeaderName = "clipperz-hashcash-tollreceipt"

createHeaders :: AS.AppState -> Array RequestHeader
createHeaders { toll, sessionKey, currentChallenge } = 
  let
    tollChallengeHeader = (\t ->   RequestHeader tollHeaderName        (show t.toll)) <$> fromMaybe currentChallenge
    tollCostHeader      = (\t ->   RequestHeader tollCostHeaderName    (show t.cost)) <$> fromMaybe currentChallenge
    tollReceiptHeader   = (\t ->   RequestHeader tollReceiptHeaderName (show t))      <$> arrayFromAsyncValue toll
    sessionHeader       = (\key -> RequestHeader sessionKeyHeaderName  (show key))    <$> fromMaybe sessionKey
  in tollChallengeHeader <> tollCostHeader <> tollReceiptHeader <> sessionHeader

-- ----------------------------------------------------------------------------

manageGenericRequest :: forall a. Url -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AS.AppError Aff (AXW.Response a)
manageGenericRequest url method body responseFormat = do
  currentState@{ toll } <- ExceptT $ liftEffect $ getAppState
  case toll of
    Done _           -> doRequest currentState
    Loading Nothing  -> doRequest currentState
    Loading (Just _) -> do
      -- small delay to prevent js single thread to block in recourive calling and let the time to the computation of the toll receipt inside forkAff to finish
      ExceptT $ Right <$> (delay $ Milliseconds 1.0) -- TODO: may be changed from busy waiting to waiting for a signal
      manageGenericRequest url method body responseFormat

  where
        doRequest :: AS.AppState -> ExceptT AS.AppError Aff (AXW.Response a)
        doRequest currentState@{ proxy } = do
          let requestInfo = case proxy of
                              OnlineProxy _ -> OnlineRequestInfo  { url 
                                                                  , method
                                                                  , headers: createHeaders currentState
                                                                  , body
                                                                  , responseFormat
                                                                  }
                              OfflineProxy  -> OfflineRequestInfo { url, method, body, responseFormat }
          response <- withExceptT (\e -> AS.ProtocolError e) (ExceptT $ doGenericRequest proxy requestInfo)
          manageResponse response.status response

        manageResponse :: StatusCode -> (AXW.Response a -> ExceptT AS.AppError Aff (AXW.Response a))
        manageResponse code@(StatusCode n)
          | n == 402            = \response -> do
              case (extractChallenge response.headers) of
                Just challenge -> do
                  hashFunc <- ExceptT $ liftEffect $ ((<$>) getHashFunctionFromAppState) <$> getAppState
                  receipt <- ExceptT $ Right <$> computeReceipt hashFunc challenge --TODO change hash function with the one in state
                  ExceptT $ updateAppState { toll: Done receipt, currentChallenge: Just challenge }
                  manageGenericRequest url method body responseFormat
                Nothing -> except $ Left $  AS.ProtocolError $ IllegalResponse "HashCash headers not present or wrong"
          | isStatusCodeOk code = \response -> do     
              -- change the toll in state to Loading so to let know to the next request to wait for the result
              currentState@{ toll } <- ExceptT $ liftEffect $ getAppState
              ExceptT $ Right <$> modifyAppState (currentState { toll = toLoading toll })
              
              case (extractChallenge response.headers) of
                Just challenge -> do
                  -- compute the new toll in forkAff to keep the program going
                  ExceptT $ Right <$> (void $ forkAff $ runExceptT $ do 
                    hashFunc <- ExceptT $ liftEffect $ ((<$>) getHashFunctionFromAppState) <$> getAppState
                    receipt <- ExceptT $ Right <$> computeReceipt hashFunc challenge --TODO change hash function with the one in state
                    appState <- ExceptT $ liftEffect getAppState
                    if appState.c == Nothing then -- logout or delete done
                      except $ Right unit
                    else
                      ExceptT $ updateAppState { toll: Done receipt, currentChallenge: Just challenge }
                  )
                  pure response
                Nothing -> pure response
          | otherwise           = \_ -> do
              ExceptT $ updateAppState { toll: Loading Nothing }
              except $ Left $ AS.ProtocolError $ ResponseError n
        
        extractChallenge :: Array ResponseHeader -> Maybe TollChallenge
        extractChallenge headers =
          let tollArray = filter (\a -> name a == tollHeaderName) headers
              costArray = filter (\a -> name a == tollCostHeaderName) headers
          in case tollArray, costArray of
              [tollHeader], [costHeader] -> (\cost -> { toll: hex $ value tollHeader, cost }) <$> fromString (value costHeader)
              _, _                               -> Nothing

doGenericRequest :: forall a. Proxy -> RequestInfo a -> Aff (Either ProtocolError (AXW.Response a))
doGenericRequest (OnlineProxy baseUrl) (OnlineRequestInfo { url, method, headers, body, responseFormat }) =
  lmap (\e -> RequestError e) <$> AXW.request (
    AXW.defaultRequest {
        url            = joinWith "/" [baseUrl, url]
      , method         = Left method
      , headers        = headers
      , content        = body 
      , responseFormat = responseFormat
    }
  )
doGenericRequest  OfflineProxy   (OfflineRequestInfo { url: _, method: _, body: _, responseFormat: _ }) =
  pure $ Left $ ResponseError 500 -- TODO
doGenericRequest (OnlineProxy _) (OfflineRequestInfo _) = pure $ Left $ IllegalRequest "Cannot do an offline request with an online proxy"
doGenericRequest  OfflineProxy   (OnlineRequestInfo  _) = pure $ Left $ IllegalRequest "Cannot do an online request with an offline proxy"

data RequestInfo a = OnlineRequestInfo  { url :: Url 
                                        , method :: Method
                                        , headers :: Array RequestHeader
                                        , body :: Maybe RequestBody
                                        , responseFormat :: RF.ResponseFormat a
                                        } 
                   | OfflineRequestInfo { url :: Url 
                                        , method :: Method
                                        , body :: Maybe RequestBody
                                        , responseFormat :: RF.ResponseFormat a
                                        }

isStatusCodeOk :: StatusCode -> Boolean
isStatusCodeOk code = (code >= (StatusCode 200)) && (code <= (StatusCode 299))
