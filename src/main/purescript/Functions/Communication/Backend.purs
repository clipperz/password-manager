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

import Affjax.RequestBody (RequestBody(..), json)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AXW
import Control.Alt ((<#>))
import Control.Alternative ((*>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Category ((<<<), (>>>))
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Data.Argonaut.Core (stringify)
import Data.Array (filter, init, last)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt)
import Data.Boolean (otherwise)
import Data.Codec.Argonaut (decode, encode)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, fromArrayBuffer, fromBigInt, hex, hexStringCodec, toArrayBuffer, toBigInt)
import Data.HeytingAlgebra ((&&))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ord ((<=), (>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith, split)
import Data.String.Pattern (Pattern(..))
import Data.Time.Duration (Milliseconds(..), Seconds(..), fromDuration)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (fromMaybe)
import Data.Unit (Unit, unit)
import DataModel.AppError (AppError(..))
import DataModel.AppState (BackendSessionState, Proxy(..), ProxyResponse(..))
import DataModel.AsyncValue (AsyncValue(..), arrayFromAsyncValue)
import DataModel.Communication.FromString (class FromString)
import DataModel.Communication.FromString as BCFS
import DataModel.Communication.Login (LoginStep2Response, loginStep1ResponseCodec, loginStep2ResponseCodec)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRPVersions.SRP (HashFunction, SRPConf)
import DataModel.UserVersions.User (MasterKey, RequestUserCard(..), requestUserCardCodec)
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.ArrayBuffer (arrayBufferToBigInt)
import Functions.HashCash (TollChallenge, computeReceipt)
import Functions.SRP as SRP
import Prim.Row (class Nub, class Union)
import Record (merge)

foreign import _readBlob     :: String -> String
foreign import _readUserCard :: Unit   -> String

-- ==================================================

type Url = String
type Path = String
type SessionKey = HexString

type ConnectionState = {
  proxy    :: Proxy
, hashFunc :: HashFunction
, srpConf  :: SRPConf
, c        :: HexString
, p        :: HexString
}

loginStep1RequestCodec :: CA.JsonCodec {c :: HexString, aa :: HexString}
loginStep1RequestCodec = 
  CAR.object "loginStep1Request" 
    { c:  hexStringCodec
    , aa: hexStringCodec
    }

loginStep2RequestCodec :: CA.JsonCodec {m1 :: HexString}
loginStep2RequestCodec = 
  CAR.object "loginStep2Request"
    { m1: hexStringCodec
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

loginRequest :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
loginRequest = requestWithoutAuthorization

signupRequest :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
signupRequest = requestWithoutAuthorization

shareRequest :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
shareRequest = requestWithoutAuthorization

genericRequest :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
genericRequest connectionState path method body responseFormat =
  (manageGenericRequestAndResponse connectionState path method body responseFormat) # (manageAuth connectionState path method body responseFormat)

requestWithoutAuthorization :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
requestWithoutAuthorization connectionState path method body responseFormat =
   manageGenericRequestAndResponse connectionState path method body responseFormat

-- ------------------------

manageGenericRequestAndResponse :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
manageGenericRequestAndResponse connectionState@{proxy, hashFunc, srpConf} path method body responseFormat = do
  case proxy of
    (OnlineProxy baseUrl tollManager _) -> 
      case tollManager.toll of
        Loading (Just _)  -> (liftAff $ delay $ Milliseconds 1.0) *> -- small delay to prevent js single thread to block in recourive calling and let the time to the computation of the toll receipt inside forkAff to finish
                              manageGenericRequestAndResponse connectionState path method body responseFormat 
        _                 ->  withExceptT ProtocolError (doOnlineRequest  baseUrl) >>= manageOnlineResponse
    (StaticProxy session) ->  withExceptT ProtocolError (doOfflineRequest session)
  
  where 
  
    doOnlineRequest :: String -> ExceptT ProtocolError Aff (AXW.Response a)
    doOnlineRequest baseUrl =
      ExceptT $ lmap (\e -> RequestError e) <$> AXW.request (
        AXW.defaultRequest {
            url            = joinWith "/" [baseUrl, path]
          , method         = Left method
          , headers        = createHeaders proxy
          , timeout        = Just $ fromDuration (Seconds 10.0)
          , content        = body
          , responseFormat = responseFormat
        }
      )

    manageOnlineResponse :: AXW.Response a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
    manageOnlineResponse response@{status}
      | status == StatusCode 402 =
          case extractChallenge response.headers, extractSession response.headers of
            Just challenge, Just session -> do
                  receipt <- liftAff $ computeReceipt hashFunc challenge
                  genericRequest connectionState { proxy = (updateToll { toll: Done receipt, currentChallenge: Just challenge } >>> updateSession (Just session)) proxy } path method body responseFormat
            _, _ ->
                  throwError $ ProtocolError (IllegalResponse "HashCash and Session headers not present or wrong")
      | isStatusCodeOk status =
          case extractChallenge response.headers, extractSession response.headers of
            Just challenge, Just session -> do
                  receipt  <- liftAff $ computeReceipt hashFunc challenge --TODO this is not async anymore
                  pure $ ProxyResponse (updateToll { toll: Done receipt   , currentChallenge: Just challenge } >>> updateSession (Just session) $ proxy) response
            _, _ ->
                  pure $ ProxyResponse (updateToll { toll: Loading Nothing, currentChallenge: Nothing        } >>> updateSession  Nothing       $ proxy) response
      | otherwise =
                  throwError $ ProtocolError (ResponseError (unwrap status))

    doOfflineRequest :: Maybe BackendSessionState -> ExceptT ProtocolError Aff (ProxyResponse (AXW.Response a))
    doOfflineRequest sessionState =
      let pieces = split (Pattern "/") path
          type' = (joinWith "/") <$> (init pieces)
          ref' = last pieces      
      in case method of
        GET -> case type', ref' of
          Just "blobs", Just ref -> do
            result <- liftEffect $ BCFS.fromString $ _readBlob ref
            pure $ ProxyResponse (StaticProxy sessionState) (responseOk result)

          Just "users", Just _   -> do
            result <- liftEffect $ BCFS.fromString $ _readUserCard unit
            pure $ ProxyResponse (StaticProxy sessionState) (responseOk result)
          
          Nothing, _       -> throwError $ IllegalRequest ("Malformed url: " <> path)
          _      , Nothing -> throwError $ IllegalRequest ("Malformed url: " <> path)
          _      , _       -> throwError $ ResponseError 501

        POST -> case type', ref', body of
          Just "login/step1", Just _, (Just (Json step)) -> do
            {aa}       <- except  $  decode loginStep1RequestCodec step # lmap (DecodeError <<< show)
            uc         <- ExceptT $ (BCFS.fromString $ _readUserCard unit) <#> (decode requestUserCardCodec >>> lmap (DecodeError <<< show)) # liftEffect
            {s, bb, b} <-            offlineLoginStep1 uc
            response   <- liftEffect $ BCFS.fromString (stringify $ encode loginStep1ResponseCodec {s, bb})
            pure $ ProxyResponse (StaticProxy $ Just {b, aa, bb}) (responseOk response)

          Just "login/step2", Just _, (Just (Json step)) -> do
            {m1}            <- except     $  decode loginStep2RequestCodec step # lmap (DecodeError <<< show)
            uc              <- ExceptT    $ (BCFS.fromString $ _readUserCard unit) <#> (decode requestUserCardCodec >>> lmap (DecodeError <<< show)) # liftEffect
            maybeResponse   <-               offlineLoginStep2 uc m1 sessionState
            encodedResponse <- liftEffect $  sequence (encodeResponse loginStep2ResponseCodec <$> maybeResponse)
            case encodedResponse of
              (Just response) -> pure       $ ProxyResponse (StaticProxy sessionState) (responseOk response)
              (Nothing)       -> throwError $ ResponseError 400 -- TODO

          _, Just "logout", _ -> emptyResponse (StaticProxy Nothing)
          
          Nothing, _       , _ -> throwError $ IllegalRequest ("Malformed url: " <> path)
          _      , Nothing , _ -> throwError $ IllegalRequest ("Malformed url: " <> path)
          _      , _       , _ -> throwError $ ResponseError 501
        
        _   -> throwError $ ResponseError 501

      where
        encodeResponse :: forall b. CA.JsonCodec b -> (b -> Effect a)
        encodeResponse codec = encode codec >>> stringify >>> BCFS.fromString

        emptyResponse :: Proxy -> ExceptT ProtocolError Aff (ProxyResponse (AXW.Response a))
        emptyResponse proxy' = do
          emptyResponseBody <- liftEffect $ BCFS.fromString $ stringify $ encode CA.string ""
          pure $ ProxyResponse proxy' (responseOk emptyResponseBody)

        responseOk :: a -> AXW.Response a
        responseOk responseBody = { body: responseBody, headers: [], status: StatusCode 200, statusText: "OK" }

        offlineLoginStep1 :: RequestUserCard -> ExceptT ProtocolError Aff { s :: HexString, bb :: HexString, b :: HexString }
        offlineLoginStep1 (RequestUserCard {v, s}) = do
          v'         <- except  $     toBigInt v           #  note (SRPError "Cannot covert v from HexString to BigInt") 
          Tuple b bb <- ExceptT $ SRP.prepareB srpConf v' <#> lmap (SRPError <<< show)
          pure { s, bb: fromBigInt bb, b: fromBigInt b }

        offlineLoginStep2 :: RequestUserCard -> HexString -> Maybe BackendSessionState -> ExceptT ProtocolError Aff (Maybe LoginStep2Response)
        offlineLoginStep2  _                                     _    Nothing             = throwError $ ResponseError 501
        offlineLoginStep2 (RequestUserCard {v, c, s, masterKey}) m1' (Just { b, bb, aa }) = do
          let m1 = toArrayBuffer m1'
          b'      <- except  $     toBigInt b                #  note (SRPError "Cannot covert b from HexString to BigInt") 
          bb'     <- except  $     toBigInt bb               #  note (SRPError "Cannot covert b from HexString to BigInt") 
          aa'     <- except  $     toBigInt aa               #  note (SRPError "Cannot covert b from HexString to BigInt") 
          v'      <- except  $     toBigInt v                #  note (SRPError "Cannot covert v from HexString to BigInt") 
          u       <- ExceptT $ SRP.prepareU srpConf aa' bb' <#> lmap (SRPError <<< show)
          secret  <- pure    $ SRP.computeSServer srpConf aa' v' b' u
          kk      <- liftAff $ SRP.prepareK srpConf secret
          check   <- liftAff $ SRP.checkM1  srpConf c s aa' bb' kk m1
          if check 
          then do
            m2 <- liftAff $ fromArrayBuffer <$> (SRP.prepareM2 srpConf aa' m1 kk)
            pure $  Just { m2, masterKey }
          else pure Nothing

manageAuth :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AppError Aff (ProxyResponse (AXW.Response a)) -> ExceptT AppError Aff (ProxyResponse (AXW.Response a))
manageAuth connectionState@{ srpConf, c, p } path method body responseFormat exceptT = do
  res <- liftAff $ runExceptT exceptT
  case res of
    Right response -> pure response
    Left err' -> case err' of
      ProtocolError (ResponseError 401) -> do
        -- this implementation is copied from Functions.Communication.Login functions to avoid circular dependency
        -- login step1
        (Tuple a aa) <- withExceptT (\err -> ProtocolError $ SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
        let urlStep1  = joinWith "/" ["login", "step1", show c]
        let bodyStep1 = json $ encode loginStep1RequestCodec { c, aa: fromBigInt aa } :: RequestBody
        ProxyResponse proxy' step1Response <- loginRequest connectionState urlStep1 POST (Just bodyStep1) RF.json
        {s, bb: bb_} :: {s :: HexString, bb :: HexString} <- if isStatusCodeOk step1Response.status
                                                          then except     $ (decode loginStep1ResponseCodec step1Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err) 
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
        let urlStep2  = joinWith "/" ["login", "step2", show c]
        let bodyStep2 = json $ encode loginStep2RequestCodec { m1: fromArrayBuffer m1 } :: RequestBody
        ProxyResponse proxy'' step2Response <- loginRequest connectionState{proxy = proxy'} urlStep2 POST (Just bodyStep2) RF.json
        {m2, masterKey: _} :: {m2 :: HexString, masterKey :: MasterKey} <-  if isStatusCodeOk step2Response.status
                                                                            then except $     (decode loginStep2ResponseCodec step2Response.body) # lmap (\err -> ProtocolError $ DecodeError $ show err)
                                                                            else throwError $  ProtocolError $ ResponseError (unwrap step2Response.status)
        
        --
        result <- liftAff $ SRP.checkM2 srpConf aa m1 kk (toArrayBuffer m2) 
        
        if result
        then genericRequest connectionState{proxy = proxy''} path method body responseFormat
        else throwError $ ProtocolError (SRPError "Client M2 doesn't match with server M2")
      
      _ -> throwError $ err'

isStatusCodeOk :: StatusCode -> Boolean
isStatusCodeOk code = (code >= (StatusCode 200)) && (code <= (StatusCode 299))


-- ----------------------------------------------------------------------------------------
-- PRIVATE HELPER FUNCTIONS
-- ----------------------------------------------------------------------------------------

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
  let tollArray = filter (\a -> name a == tollHeaderName)     headers
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
