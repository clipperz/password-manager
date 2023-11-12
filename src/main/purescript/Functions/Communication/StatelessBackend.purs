module Functions.Communication.StatelessBackend where

import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat as RF
import Affjax.ResponseHeader (ResponseHeader, name, value)
import Affjax.StatusCode (StatusCode(..))
import Affjax.Web as AXW
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), throwError, withExceptT)
import Data.Array (filter)
import Data.Bifunctor (lmap)
import Data.Boolean (otherwise)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method)
import Data.HexString (HexString, hex)
import Data.HeytingAlgebra ((&&))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Ord ((<=), (>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Unfoldable (fromMaybe)
import Data.Unit (Unit)
import DataModel.AppState as AS
import DataModel.AsyncValue (AsyncValue(..), arrayFromAsyncValue)
import DataModel.Communication.FromString (class FromString)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.SRP (HashFunction)
import DataModel.StatelessAppState (Proxy(..), ProxyResponse(..))
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Functions.HashCash (TollChallenge, computeReceipt)
import Record (merge)

-- ==================================================

type Url = String
type Path = String
type SessionKey = HexString

type ConnectionState = {
  proxy    :: Proxy
, hashFunc :: HashFunction
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

manageGenericRequest :: forall a. FromString a => ConnectionState -> Path -> Method -> Maybe RequestBody -> RF.ResponseFormat a -> ExceptT AS.AppError Aff (ProxyResponse (AXW.Response a))
manageGenericRequest connectionState@{ proxy, hashFunc } path method body responseFormat = do
  case proxy of
    (OnlineProxy baseUrl tollManager _) -> do
       case tollManager.toll of
        Loading (Just _) -> do
          -- small delay to prevent js single thread to block in recourive calling and let the time to the computation of the toll receipt inside forkAff to finish
          liftAff $ delay $ Milliseconds 1.0 -- TODO: may be changed from busy waiting to waiting for a signal
          manageGenericRequest connectionState path method body responseFormat
        _                -> do -- Loading Nothing || Done _
          response <- withExceptT AS.ProtocolError (doOnlineRequest baseUrl)
          manageResponse response.status response
          

    (StaticProxy _) -> do
      -- withExceptT AS.ProtocolError $ doOfflineRequest session
      throwError $ AS.ProtocolError (ResponseError 500)
  
  where
    manageResponse :: StatusCode -> (AXW.Response a -> ExceptT AS.AppError Aff (ProxyResponse (AXW.Response a)))
    manageResponse code@(StatusCode n)
      | n == 402          = \response -> -- TODO: improve
          case (extractChallenge response.headers) of
            Just challenge -> do
                  receipt <- liftAff $ computeReceipt hashFunc challenge
                  manageGenericRequest { proxy: (updateToll proxy { toll: Done receipt, currentChallenge: Just challenge }), hashFunc } path method body responseFormat
            Nothing -> throwError $ AS.ProtocolError (IllegalResponse "HashCash headers not present or wrong")
      | isStatusCodeOk code = \(response :: AXW.Response a) ->
          case (extractChallenge response.headers) of
            Nothing -> 
                  pure $ ProxyResponse (updateToll proxy { toll: Loading Nothing, currentChallenge: Nothing }) response
            Just challenge -> do
                  receipt  <- liftAff $ computeReceipt hashFunc challenge --TODO this is not async anymore
                  pure $ ProxyResponse (updateToll proxy { toll: Done receipt, currentChallenge: Just challenge }) response
      | otherwise           = \_ ->
          throwError $ AS.ProtocolError (ResponseError n)

    updateToll (OnlineProxy baseUrl oldTollManager sessionKey) tollManager = OnlineProxy baseUrl (merge oldTollManager tollManager) sessionKey
    updateToll offline@(StaticProxy _)                         _           = offline
    
    extractChallenge :: Array ResponseHeader -> Maybe TollChallenge
    extractChallenge headers =
      let tollArray = filter (\a -> name a == tollHeaderName) headers
          costArray = filter (\a -> name a == tollCostHeaderName) headers
      in case tollArray, costArray of
          [tollHeader], [costHeader] -> (\cost -> { toll: hex $ value tollHeader, cost }) <$> fromString (value costHeader)
          _,             _           -> Nothing

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
  
