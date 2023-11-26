module Functions.Communication.BackendCommunication
  ( RequestInfo(..)
  , Url
  , createHeaders
  -- , doGenericRequest
  , isStatusCodeOk
  , manageGenericRequest
  , sessionKeyHeaderName
  , tollCostHeaderName
  , tollHeaderName
  , tollReceiptHeaderName
  )
  where

import Affjax.RequestBody (RequestBody(..))
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AXW
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), except, runExceptT, throwError, withExceptT)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Array (filter, last, init)
import Data.Bifunctor (lmap)
import Data.Boolean (otherwise)
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$>), void)
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, toBigInt, fromBigInt, hex, toArrayBuffer, fromArrayBuffer)
import Data.HeytingAlgebra ((&&))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Ord ((<=), (>=))
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
import DataModel.Communication.Login (LoginStep2Response)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.ProxyType (ProxyType(..), BackendSessionState(..), BackendSessionRecord)
import DataModel.User (RequestUserCard(..))
import Effect.Aff (Aff, forkAff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.HashCash (TollChallenge, computeReceipt)
import Functions.JSState (getAppState, saveAppState, updateAppState)
import Functions.SRP as SRP
import Functions.State (getSRPConf, getHashFunctionFromAppState)
import Record (merge)


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
      liftAff $ delay $ Milliseconds 1.0 -- TODO: may be changed from busy waiting to waiting for a signal
      manageGenericRequest url method body responseFormat

  where
    doRequest :: AS.AppState -> ExceptT AS.AppError Aff (AXW.Response a)
    doRequest currentState@{ proxy, toll } = do
      let requestInfo = case proxy of
                          OnlineProxy _  -> OnlineRequestInfo { url 
                                                              , method
                                                              , headers: createHeaders currentState
                                                              , body
                                                              , responseFormat
                                                              }
                          OfflineProxy _ -> OfflineRequestInfo { url, method, body, responseFormat }
      response <- withExceptT (\e -> AS.ProtocolError e) (ExceptT $ doGenericRequest proxy requestInfo)
      -- change toll to loading state because it has been used
      liftAff $ liftEffect $ saveAppState (currentState { toll = toLoading toll })
      manageResponse response.status response

    manageResponse :: StatusCode -> (AXW.Response a -> ExceptT AS.AppError Aff (AXW.Response a))
    manageResponse code@(StatusCode n)
      | n == 402          = \response -> do -- TODO: improve
          case (extractChallenge response.headers) of
            Just challenge -> do
              hashFunc <- ExceptT $ liftEffect $ ((<$>) getHashFunctionFromAppState) <$> getAppState
              receipt <- liftAff $ computeReceipt hashFunc challenge --TODO change hash function with the one in state
              ExceptT $ updateAppState { toll: Done receipt, currentChallenge: Just challenge }
              manageGenericRequest url method body responseFormat
            Nothing -> except $ Left $  AS.ProtocolError $ IllegalResponse "HashCash headers not present or wrong"
      | isStatusCodeOk code = \response -> do     
          -- change the toll in state to Loading so to let know to the next request to wait for the result
          case (extractChallenge response.headers) of
            Just challenge -> do
              -- compute the new toll in forkAff to keep the program going
              liftAff $ void $ forkAff $ runExceptT $ do 
                hashFunc <- ExceptT $ liftEffect $ ((<$>) getHashFunctionFromAppState) <$> getAppState
                receipt  <- liftAff $ computeReceipt hashFunc challenge --TODO change hash function with the one in state
                appState <- ExceptT $ liftEffect getAppState
                if appState.c == Nothing then -- logout or delete done
                  pure unit
                else
                  ExceptT $ updateAppState { toll: Done receipt, currentChallenge: Just challenge }
              pure response
            Nothing -> do
              ExceptT $ updateAppState { toll: Loading Nothing, currentChallenge: Nothing }
              pure response
      | otherwise           = \_ -> do
          ExceptT $ updateAppState { toll: Loading Nothing }
          throwError $ AS.ProtocolError (ResponseError n)
    
    extractChallenge :: Array ResponseHeader -> Maybe TollChallenge
    extractChallenge headers =
      let tollArray = filter (\a -> name a == tollHeaderName) headers
          costArray = filter (\a -> name a == tollCostHeaderName) headers
      in case tollArray, costArray of
          [tollHeader], [costHeader] -> (\cost -> { toll: hex $ value tollHeader, cost }) <$> fromString (value costHeader)
          _,             _           -> Nothing

    doGenericRequest :: FromString a => ProxyType -> RequestInfo a -> Aff (Either ProtocolError (AXW.Response a))
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
    doGenericRequest (OfflineProxy (BackendSessionState session)) (OfflineRequestInfo { url, method, body, responseFormat: _ }) =
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
            Just "users", Just _   -> do
              result <- liftEffect $ BCFS.fromString $ _readUserCard unit
              pure $ Right $ { body: result, headers: [], status: StatusCode 200, statusText: "OK"}
            _           , _        -> pure $ Left $ ResponseError 501
        POST -> do
          case type', ref', body of
            Nothing  , _       , _       -> pure $ Left $ IllegalRequest $ "Malformed url: " <> url
            _        , Nothing , _       -> pure $ Left $ IllegalRequest $ "Malformed url: " <> url
            Just "login/step1", Just _, (Just (Json step)) -> do
              res :: Either AS.AppError a <- runExceptT $ do
                stepData :: { c :: HexString, aa :: HexString } <- except $ lmap (\e -> AS.ProtocolError $ DecodeError $ show e) $ decodeJson step 
                uc <- ExceptT $ (\v -> lmap (\e -> AS.ProtocolError $ DecodeError $ show e) $ decodeJson v) <$> (liftEffect $ BCFS.fromString $ _readUserCard unit)
                {s, bb, b} <- offlineLoginStep1 uc
                let newSession = BackendSessionState $ merge { b: Just b, aa: Just stepData.aa , bb: Just bb} session
                ExceptT $ updateAppState { proxy: OfflineProxy newSession }
                let jsonString = stringify $ encodeJson {s, bb}
                liftAff $ liftEffect $ BCFS.fromString jsonString
              case res of
                Left err -> do
                  log $ "login/step1" <> (show err)
                  pure $ Left $ ResponseError 400 -- TODO
                Right a -> pure $ Right $ { body: a, headers: [], status: StatusCode 200, statusText: "OK" }
            Just "login/step2", Just _, (Just (Json step)) -> do
              res :: Either AS.AppError (Maybe a) <- runExceptT $ do
                stepData :: { m1 :: HexString } <- except $ lmap (\e -> AS.ProtocolError $ DecodeError $ show e) $ decodeJson step
                uc <- ExceptT $ (\v -> lmap (\e -> AS.ProtocolError $ DecodeError $ show e) $ decodeJson v) <$> (liftEffect $ BCFS.fromString $ _readUserCard unit)
                mResponse <- offlineLoginStep2 uc stepData.m1 session
                let mJsonString = (stringify <<< encodeJson) <$> mResponse
                case mJsonString of
                  Nothing -> except $ Right Nothing
                  Just jsonString -> liftAff $ Just <$> (liftEffect $ BCFS.fromString jsonString)
              case res of
                Left err -> do
                  log $ "login/step2" <> (show err)
                  pure $ Left $ ResponseError 400 -- TODO
                Right Nothing -> do
                  log $ "login/step2 error in mJsonString"
                  pure $ Left $ ResponseError 400 -- TODO
                Right (Just a) -> pure $ Right $ { body: a, headers: [], status: StatusCode 200, statusText: "OK" }
            Just "logout", _, _ -> do
              res <- runExceptT $ do
                let newSession = BackendSessionState { b: Nothing, bb: Nothing, aa: Nothing }
                ExceptT $ updateAppState { proxy: OfflineProxy newSession }
                liftAff $ liftEffect $ BCFS.fromString ""
              case res of 
                Left _ ->  pure $ Left  $ ResponseError 500
                Right a -> pure $ Right $ { body: a, headers: [], status: StatusCode 200, statusText: "OK" }
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


offlineLoginStep1 :: RequestUserCard -> ExceptT AS.AppError Aff { s :: HexString, bb :: HexString, b :: HexString }
offlineLoginStep1 (RequestUserCard r) = do
  srpConf <- ExceptT $ liftEffect getSRPConf
  v <- except $ note (AS.ProtocolError $ SRPError "Cannot covert v from HexString to BigInt") (toBigInt r.v)
  (Tuple b bb) <- ExceptT $ (lmap (\e -> AS.ProtocolError $ SRPError $ show e)) <$> (SRP.prepareB srpConf v)
  except $ Right $ { s: r.s, bb: fromBigInt bb, b: fromBigInt b }

offlineLoginStep2 :: RequestUserCard -> HexString -> BackendSessionRecord -> ExceptT AS.AppError Aff (Maybe LoginStep2Response)
offlineLoginStep2 (RequestUserCard r) m1' { b: mb, bb: mbb, aa: maa } = do
  let m1 = toArrayBuffer m1'
  srpConf <- ExceptT $ liftEffect getSRPConf
  b'      <- except  $  mb            # note (AS.InvalidStateError $ AS.MissingValue "b not in session") 
  bb'     <- except  $  mbb           # note (AS.InvalidStateError $ AS.MissingValue "b not in session") 
  aa'     <- except  $  maa           # note (AS.InvalidStateError $ AS.MissingValue "b not in session") 
  b       <- except  $ (toBigInt b')  # note (AS.ProtocolError     $ SRPError "Cannot covert b from HexString to BigInt") 
  bb      <- except  $ (toBigInt bb') # note (AS.ProtocolError     $ SRPError "Cannot covert b from HexString to BigInt") 
  aa      <- except  $ (toBigInt aa') # note (AS.ProtocolError     $ SRPError "Cannot covert b from HexString to BigInt") 
  v       <- except  $ (toBigInt r.v) # note (AS.ProtocolError     $ SRPError "Cannot covert v from HexString to BigInt") 
  u       <- ExceptT $ (lmap (\e -> AS.ProtocolError $ SRPError $ show e)) <$> SRP.prepareU srpConf aa bb 
  secret  <- pure    $ SRP.computeSServer srpConf aa v b u
  kk      <- liftAff $ SRP.prepareK srpConf secret
  check   <- liftAff $ SRP.checkM1 srpConf r.c r.s aa bb kk m1
  if check then do
    m2 <- liftAff $ fromArrayBuffer <$> (SRP.prepareM2 srpConf aa m1 kk)
    pure $ Just { m2, masterKey: r.masterKey }
  else pure Nothing
  
