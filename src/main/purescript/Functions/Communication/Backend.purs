module Functions.Communication.Backend
  ( ConnectionState
  , Path
  , SessionKey
  , Url
  , genericRequest
  , isStatusCodeOk
  , loginRequest
  , shareRequest
  , signupRequest
  )
  where

import Affjax.RequestBody (RequestBody, json)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AXW
import Control.Alt ((<#>))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Category ((>>>))
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (filter)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt)
import Data.Boolean (otherwise)
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer, fromBigInt, hex, toArrayBuffer, toBigInt)
import Data.HeytingAlgebra ((&&))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ord ((<=), (>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (Proxy(..), ProxyResponse(..))
import DataModel.AsyncValue (AsyncValue(..), arrayFromAsyncValue)
import DataModel.Communication.FromString (class FromString)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Credentials (Credentials)
import DataModel.SRP (SRPConf, HashFunction)
import DataModel.User (MasterKey)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Functions.ArrayBuffer (arrayBufferToBigInt)
import Functions.HashCash (TollChallenge, computeReceipt)
import Functions.SRP as SRP
import Prim.Row (class Nub, class Union)
import Record (merge)

-- ==================================================

type Url = String
type Path = String
type SessionKey = HexString

type ConnectionState = {
  proxy       :: Proxy
, hashFunc    :: HashFunction
, srpConf     :: SRPConf
, credentials :: Credentials
}

-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-usersession-id"

tollHeaderName :: String
tollHeaderName = "clipperz-hashcash-tollchallenge"

tollCostHeaderName :: String
tollCostHeaderName = "clipperz-hashcash-tollcost"

tollReceiptHeaderName :: String
tollReceiptHeaderName = "clipperz-hashcash-tollreceipt"

createHeaders :: Proxy -> Array RequestHeader
createHeaders (OnlineProxy _ { toll, currentChallenge } sessionKey) = 
  let
    tollChallengeHeader = (\t ->   RequestHeader tollHeaderName        (show t.toll)) <$> fromMaybe currentChallenge
    tollCostHeader      = (\t ->   RequestHeader tollCostHeaderName    (show t.cost)) <$> fromMaybe currentChallenge
    tollReceiptHeader   = (\t ->   RequestHeader tollReceiptHeaderName (show t))      <$> arrayFromAsyncValue toll
    sessionHeader       = (\key -> RequestHeader sessionKeyHeaderName  (show key))    <$> fromMaybe sessionKey
  in tollChallengeHeader <> tollCostHeader <> tollReceiptHeader <> sessionHeader
createHeaders (StaticProxy _) = []

-- ----------------------------------------------------------------------------

foreign import _readBlob :: String -> String

foreign import _readUserCard :: Unit -> String

requestWithoutAuthorization :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
requestWithoutAuthorization connectionState@{proxy} path method body responseFormat = 
  (manageGenericRequest proxy path method body responseFormat) >>= (manageGenericResponse connectionState path method body responseFormat)

loginRequest :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
loginRequest = requestWithoutAuthorization

signupRequest :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
signupRequest = requestWithoutAuthorization

shareRequest :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
shareRequest = requestWithoutAuthorization

genericRequest :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
genericRequest connectionState@{proxy} path method body responseFormat =
  ((manageGenericRequest proxy path method body responseFormat) >>= (manageGenericResponse connectionState path method body responseFormat)) # (manageAuth connectionState path method body responseFormat)

manageGenericRequest :: forall a. FromString a => Proxy -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (AXW.Response a)
manageGenericRequest proxy path method body responseFormat = do
  case proxy of
    (OnlineProxy baseUrl tollManager _) -> do
       case tollManager.toll of
        Loading (Just _) -> do
          -- small delay to prevent js single thread to block in recourive calling and let the time to the computation of the toll receipt inside forkAff to finish
          liftAff $ delay $ Milliseconds 1.0 -- TODO: may be changed from busy waiting to waiting for a signal
          manageGenericRequest proxy path method body responseFormat
        _                -> do -- Loading Nothing || Done _
          withExceptT ProtocolError (doOnlineRequest baseUrl)
          -- manageResponse response.status response
          

    (StaticProxy _) -> do
      -- withExceptT ProtocolError $ doOfflineRequest session
      throwError $ ProtocolError (ResponseError 500)
  
  where
    doOnlineRequest :: String -> ExceptT ProtocolError Aff (AXW.Response a)
    doOnlineRequest baseUrl =
      ExceptT $ lmap (\e -> RequestError e) <$> AXW.request (
        AXW.defaultRequest {
            url            = joinWith "/" [baseUrl, path]
          , method         = Left method
          , headers        = createHeaders proxy
          , content        = body
          , responseFormat = responseFormat
        }
      )


manageAuth :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a)) -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
manageAuth connectionState@{ srpConf, credentials: {username, password} } path method body responseFormat exceptT = do
  res <- liftAff $ runExceptT exceptT
  case res of
    Right response -> pure response
    Left err' -> case err' of
      ProtocolError (ResponseError 401) -> do
        -- this implementation is copied from Functions.Communication.Login functions to avoid circular dependency
        -- prepare login
        c         <- liftAff $ fromArrayBuffer <$> SRP.prepareC srpConf username password
        p         <- liftAff $ fromArrayBuffer <$> SRP.prepareP srpConf username password
        
        -- login step1
        (Tuple a aa) <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
        let urlStep1  = joinWith "/" ["login", "step1", show c] :: String
        let bodyStep1 = json $ encodeJson { c, aa: fromBigInt aa }  :: RequestBody
        ProxyResponse proxy' step1Response <- loginRequest connectionState urlStep1 POST (Just bodyStep1) RF.json
        {s, bb: bb_} :: {s :: HexString, bb :: HexString} <- if isStatusCodeOk step1Response.status
                                                          then except     $ (decodeJson step1Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err) 
                                                          else throwError $  ProtocolError (ResponseError (unwrap step1Response.status))
        bb :: BigInt <- except $ (toBigInt bb_) # note (ProtocolError $ SRPError "Error in converting B from String to BigInt")
        _ <-  if bb == fromInt (0)
              then throwError $ ProtocolError (SRPError "Server returned B == 0")
              else pure unit
        
        -- login step2
        x  :: BigInt      <-  ExceptT $ (srpConf.kdf srpConf.hash (toArrayBuffer s) (toArrayBuffer p)) <#> (\ab -> note (ProtocolError $ SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab))
        ss :: BigInt      <- (ExceptT $ SRP.prepareSClient srpConf aa bb x a) # withExceptT (\err -> ProtocolError $ SRPError $ show err)
        kk :: ArrayBuffer <-  liftAff $ SRP.prepareK  srpConf ss
        m1 :: ArrayBuffer <-  liftAff $ SRP.prepareM1 srpConf c s aa bb kk
        let urlStep2  = joinWith "/" ["login", "step2", show c]      :: String
        let bodyStep2 = json $ encodeJson { m1: fromArrayBuffer m1 } :: RequestBody
        ProxyResponse proxy'' step2Response <- loginRequest connectionState{proxy = proxy'} urlStep2 POST (Just bodyStep2) RF.json
        {m2, masterKey: _} :: {m2 :: HexString, masterKey :: MasterKey} <-  if isStatusCodeOk step2Response.status
                                                                            then except $     (decodeJson step2Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err)
                                                                            else throwError $  ProtocolError $ ResponseError (unwrap step2Response.status)
        
        --
        result <- liftAff $ SRP.checkM2 srpConf aa m1 kk (toArrayBuffer m2) 
        
        if result
        then genericRequest connectionState{proxy = proxy''} path method body responseFormat
        else throwError $ ProtocolError (SRPError "Client M2 doesn't match with server M2")
      
      _ -> throwError $ err'

manageGenericResponse :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> AXW.Response a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
manageGenericResponse connectionState@{proxy, hashFunc} path method body responseFormat response@{status}
  | status == StatusCode 402 =
      case extractChallenge response.headers, extractSession response.headers of
        Just challenge, Just session -> do
              receipt <- liftAff $ computeReceipt hashFunc challenge
              genericRequest connectionState { proxy = (updateToll { toll: Done receipt, currentChallenge: Just challenge } >>> updateSession (Just session)) proxy } path method body responseFormat
        _, _ -> throwError $ ProtocolError (IllegalResponse "HashCash and Session headers not present or wrong")
  | isStatusCodeOk status =
      case extractChallenge response.headers, extractSession response.headers of
        Just challenge, Just session -> do
              receipt  <- liftAff $ computeReceipt hashFunc challenge --TODO this is not async anymore
              pure $ ProxyResponse (updateToll { toll: Done receipt, currentChallenge: Just challenge } >>> updateSession (Just session) $ proxy) response
        _, _ ->
              pure $ ProxyResponse (updateToll { toll: Loading Nothing, currentChallenge: Nothing }     >>> updateSession Nothing        $ proxy) response
  | otherwise = throwError $ ProtocolError (ResponseError (unwrap status))
    
updateToll :: forall r1 r2.
     Union  r1 ( currentChallenge :: Maybe TollChallenge, toll :: AsyncValue HexString) r2
  => Nub    r2 ( currentChallenge :: Maybe TollChallenge, toll :: AsyncValue HexString)
  => Record r1 -> Proxy -> Proxy
updateToll tollManager (OnlineProxy baseUrl oldTollManager sessionKey) = OnlineProxy baseUrl (merge tollManager oldTollManager) sessionKey
updateToll _           offline@(StaticProxy _)                         = offline

updateSession :: Maybe HexString -> Proxy -> Proxy
updateSession sessionKey (OnlineProxy baseUrl tollManager _) = OnlineProxy baseUrl tollManager sessionKey
updateSession _          offline@(StaticProxy _)             = offline

extractChallenge :: Array ResponseHeader -> Maybe TollChallenge
extractChallenge headers =
  let tollArray = filter (\a -> name a == tollHeaderName) headers
      costArray = filter (\a -> name a == tollCostHeaderName) headers
  in case tollArray, costArray of
      [tollHeader], [costHeader] -> (\cost -> { toll: hex $ value tollHeader, cost }) <$> fromString (value costHeader)
      _,             _           -> Nothing

extractSession :: Array ResponseHeader -> Maybe SessionKey
extractSession headers =
  let sessionArray = filter (\a -> name a == sessionKeyHeaderName) headers
  in case sessionArray of
    [sessionHeader] -> Just $ hex (value sessionHeader)
    _               -> Nothing

    -- doOfflineRequest :: Maybe BackendSessionState -> ExceptT ProtocolError Aff (Tuple Proxy (AXW.Response a))
    -- doOfflineRequest sessionState =
    --   let pieces = split (Pattern "/") path
    --       type' = (joinWith "/") <$> (init pieces)
    --       ref' = last pieces
      
    --   in case method of
    --     GET -> do
    --       case type', ref' of
    --         Nothing  , _           -> throwError $ IllegalRequest ("Malformed url: " <> path)
    --         _        , Nothing     -> throwError $ IllegalRequest ("Malformed url: " <> path)

    --         Just "blobs", Just ref -> do
    --           result <- liftEffect $ BCFS.fromString $ _readBlob ref
    --           pure $ Tuple (OfflineProxy sessionState) { body: result, headers: [], status: StatusCode 200, statusText: "OK" }

    --         Just "users", Just _   -> do
    --           result <- liftEffect $ BCFS.fromString $ _readUserCard unit
    --           pure $ Tuple (OfflineProxy sessionState) { body: result, headers: [], status: StatusCode 200, statusText: "OK"}
            
    --         _           , _        -> throwError $ ResponseError 501

    --     POST -> do
    --       case type', ref', body of
    --         Nothing  , _       , _       -> throwError $ IllegalRequest ("Malformed url: " <> path)
    --         _        , Nothing , _       -> throwError $ IllegalRequest ("Malformed url: " <> path)

    --         Just "login/step1", Just _, (Just (Json step)) -> do
    --           stepData :: { c :: HexString, aa :: HexString } <- except $ lmap (\e -> DecodeError $ show e) $ decodeJson step
    --           uc <- ExceptT $ (\v -> lmap (\e -> DecodeError $ show e) $ decodeJson v) <$> (liftEffect $ BCFS.fromString $ _readUserCard unit)
    --           {s, bb, b} <- offlineLoginStep1 srpConf uc
    --           let newSession = BackendSessionState { b, aa: stepData.aa , bb}
    --           let jsonString = stringify $ encodeJson {s, bb}
    --           res <- liftEffect $ BCFS.fromString jsonString
    --           pure $ Tuple (OfflineProxy $ Just newSession) { body: res, headers: [], status: StatusCode 200, statusText: "OK" }

    --         Just "login/step2", Just _, (Just (Json step)) -> do
    --           stepData :: { m1 :: HexString } <- except $ lmap (\e -> DecodeError $ show e) $ decodeJson step
    --           uc <- ExceptT $ (\v -> lmap (\e -> DecodeError $ show e) $ decodeJson v) <$> (liftEffect $ BCFS.fromString $ _readUserCard unit)
    --           BackendSessionState session <- except $ note (SRPError "cannot find A, b and B") sessionState
    --           mResponse <- offlineLoginStep2 srpConf uc stepData.m1 session
    --           case ((encodeJson >>> stringify >>> BCFS.fromString) <$> mResponse) of
    --             Nothing -> do
    --               log $ "login/step2 error in mJsonString"
    --               throwError $ ResponseError 400 -- TODO
    --             (Just a) -> do
    --               res <- liftEffect $ a
    --               pure $ Tuple (OfflineProxy sessionState) { body: res, headers: [], status: StatusCode 200, statusText: "OK" }

    --         Just "logout", _, _ -> do
    --           res <- runExceptT $ do
    --             liftAff $ liftEffect $ BCFS.fromString ""
    --           case res of 
    --             Left _ ->  throwError $ ResponseError 500
    --             Right a -> pure $ Tuple (OfflineProxy Nothing) { body: a, headers: [], status: StatusCode 200, statusText: "OK" }
            
    --         _, _, _ -> throwError $ ResponseError 501
        
    --     _   -> throwError $ ResponseError 501

isStatusCodeOk :: StatusCode -> Boolean
isStatusCodeOk code = (code >= (StatusCode 200)) && (code <= (StatusCode 299))

-- offlineLoginStep1 :: SRPConf -> RequestUserCard -> ExceptT ProtocolError Aff { s :: HexString, bb :: HexString, b :: HexString }
-- offlineLoginStep1 srpConf (RequestUserCard r) = do
--   v <- except $ note (SRPError "Cannot covert v from HexString to BigInt") (toBigInt r.v)
--   (Tuple b bb) <- ExceptT $ (lmap (\e -> SRPError $ show e)) <$> (SRP.prepareB srpConf v)
--   pure { s: r.s, bb: fromBigInt bb, b: fromBigInt b }

-- offlineLoginStep2 :: SRPConf -> RequestUserCard -> HexString -> BackendSessionRecord -> ExceptT ProtocolError Aff (Maybe LoginStep2Response)
-- offlineLoginStep2 srpConf (RequestUserCard r) m1' { b, bb, aa } = do
--   let m1 = toArrayBuffer m1'
--   b'       <- except  $ (toBigInt b)  # note (SRPError "Cannot covert b from HexString to BigInt") 
--   bb'      <- except  $ (toBigInt bb) # note (SRPError "Cannot covert b from HexString to BigInt") 
--   aa'      <- except  $ (toBigInt aa) # note (SRPError "Cannot covert b from HexString to BigInt") 
--   v       <- except  $ (toBigInt r.v) # note (SRPError "Cannot covert v from HexString to BigInt") 
--   u       <- ExceptT $ (lmap (\e -> SRPError $ show e)) <$> SRP.prepareU srpConf aa' bb' 
--   secret  <- pure    $ SRP.computeSServer srpConf aa' v b' u
--   kk      <- liftAff $ SRP.prepareK srpConf secret
--   check   <- liftAff $ SRP.checkM1 srpConf r.c r.s aa' bb' kk m1
--   if check then do
--     m2 <- liftAff $ fromArrayBuffer <$> (SRP.prepareM2 srpConf aa' m1 kk)
--     pure $ Just { m2, encUserInfoReferences: fst r.masterKey }
--   else pure Nothing
  
