module Functions.Communication.BackendCommunication
  ( RequestInfo(..)
  , Url
  , createHeaders
  , doGenericRequest
  , isStatusCodeOk
  , manageGenericRequest
  , sessionKeyHeaderName
  , tollCostHeaderName
  , tollHeaderName
  , tollReceiptHeaderName
  )
  where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, withExceptT, runExceptT)
import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Array (filter, last, head, init)
import Data.Bifunctor (lmap)
import Data.Boolean (otherwise)
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>), void)
import Data.HexString (HexString, toBigInt, fromBigInt, hex, toArrayBuffer)
import Data.HeytingAlgebra ((&&))
import Data.HTTP.Method (Method(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Ord((<=), (>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith, split)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Data.Unit (unit, Unit)
import DataModel.AppState as AS
import DataModel.AsyncValue (AsyncValue(..), arrayFromAsyncValue, toLoading)
import DataModel.Communication.FromString (class FromString)
import DataModel.Communication.FromString as BCFS
import DataModel.Communication.Login (LoginStep1Response, LoginStep2Response)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Proxy (Proxy(..))
import DataModel.SRP (hashFuncSHA256)
import DataModel.User (UserCard(..))
import Effect.Aff (Aff, forkAff, delay)
import Effect (Effect)
import Effect.Class (liftEffect)
import Functions.HashCash (TollChallenge, computeReceipt)
import Functions.JSState (getAppState, modifyAppState, updateAppState)
import Functions.State (getSRPConf, getHashFunctionFromAppState)
import Functions.SRP as SRP

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

foreign import _readBlob :: String -> String

foreign import _readUserCard :: Unit -> String

manageGenericRequest :: forall a. FromString a => Url -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AS.AppError Aff (AXW.Response a)
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
                              OnlineProxy _  -> OnlineRequestInfo  { url 
                                                                  , method
                                                                  , headers: createHeaders currentState
                                                                  , body
                                                                  , responseFormat
                                                                  }
                              OfflineProxy _ -> OfflineRequestInfo { url, method, body, responseFormat }
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

doGenericRequest :: forall a. FromString a => Proxy -> RequestInfo a -> Aff (Either ProtocolError (AXW.Response a))
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
doGenericRequest (OfflineProxy session) (OfflineRequestInfo { url, method, body, responseFormat }) =
  let pieces = split (Pattern "/") url
      type' = (joinWith "/") <$> (init pieces)
      ref' = last pieces
  in case method of
    GET -> do
      case type', ref' of
        Nothing  , _           -> pure $ Left $ IllegalRequest $ "Malformed url: " <> url
        _        , Nothing     -> pure $ Left $ IllegalRequest $ "Malformed url: " <> url
        Just "blobs", Just ref -> do
          result <- liftEffect $ BCFS.fromString $ _readBlob ref
          pure $ Right $ { body: result, headers: [], status: StatusCode 200, statusText: "OK" }
        Just "users", Just c   -> do
          result <- liftEffect $ BCFS.fromString $ _readUserCard unit
          pure $ Right $ { body: result, headers: [], status: StatusCode 200, statusText: "OK"}
        _           , _        -> pure $ Left $ ResponseError 501
    POST -> do
      case type', ref', body of
        Nothing  , _       , _       -> pure $ Left $ IllegalRequest $ "Malformed url: " <> url
        _        , Nothing , _       -> pure $ Left $ IllegalRequest $ "Malformed url: " <> url
        Just "login/step1", Just ref, (Just (Json step)) -> do
          res :: Either AS.AppError a <- runExceptT $ do
            stepData :: { c :: HexString, aa :: HexString } <- except $ lmap (\e -> AS.ProtocolError $ DecodeError $ show e) $ decodeJson step 
            uc <- ExceptT $ (\v -> lmap (\e -> AS.ProtocolError $ DecodeError $ show e) $ decodeJson v) <$> (liftEffect $ BCFS.fromString $ _readUserCard unit)
            stepResult <- offlineLoginStep1 uc stepData
            let jsonString = stringify $ encodeJson stepResult
            ExceptT $ Right <$> (liftEffect $ BCFS.fromString jsonString)
          case res of
            Left err -> pure $ Left $ ResponseError 400 -- TODO
            Right res -> pure $ Right $ { body: res, headers: [], status: StatusCode 200, statusText: "OK" }
        Just "login/step2", Just ref, (Just (Json step)) -> do
          pure $ Left $ ResponseError 501 -- TODO
        _, _, _ -> pure $ Left $ ResponseError 501
    _   -> pure $ Left $ ResponseError 501
doGenericRequest (OnlineProxy  _) (OfflineRequestInfo _) = pure $ Left $ IllegalRequest "Cannot do an offline request with an online proxy"
doGenericRequest (OfflineProxy _) (OnlineRequestInfo  _) = pure $ Left $ IllegalRequest "Cannot do an online request with an offline proxy"

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


offlineLoginStep1 :: UserCard -> { c :: HexString, aa :: HexString } -> ExceptT AS.AppError Aff LoginStep1Response
offlineLoginStep1 uc@(UserCard r) { c, aa } = do
  srpConf <- ExceptT $ liftEffect getSRPConf
  v <- except $ note (AS.ProtocolError $ SRPError "Cannot covert v from HexString to BigInt") (toBigInt r.v)
  (Tuple b bb) <- ExceptT $ (lmap (\e -> AS.ProtocolError $ SRPError $ show e)) <$> (SRP.prepareB srpConf v)
  except $ Right $ { s: r.s, bb: fromBigInt bb }
