module RestBackendCommunication where

import Affjax.Web as AXW
import Affjax.RequestBody (RequestBody, json)
import Affjax.RequestHeader as RE
import Affjax.ResponseFormat as RF
import Affjax.StatusCode (StatusCode(..))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, except, withExceptT)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.BigInt (BigInt, fromInt)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&))
import Data.HTTP.Method (Method(..))
import Data.HexString (HexString, hex, toBigInt, fromBigInt, toArrayBuffer, fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Ord((<=), (>=))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import EncodeDecode (decryptJson)
import Record (merge)
import SRP as SRP
import Widgets.LoginForm as LoginForm
import Utilities (arrayBufferToBigInt)

type Url = String
type CardReference = HexString

data ProtocolError = RequestError AXW.Error | ResponseError Int | SRPError String | DecodeError String | CryptoError String
instance showProtobufRequestError :: Show ProtocolError where
  show (RequestError err) = "Request Error: "  <> AXW.printError err
  show (ResponseError i)  = "Response Error: " <> "response status code " <> show i
  show (SRPError err)     = "SRP Error: "      <> err
  show (DecodeError err)  = "Decode Error: "   <> err
  show (CryptoError err)  = "Decode Error: "   <> err

baseUrl :: Url 
baseUrl = "http://localhost:8090" --TODO: get from configuration file/build

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

-- ----------------------------------------------------------------------------

type UserCard = {
    c :: HexString
  , v :: HexString
  , s :: HexString
  , srpVersion :: String
  , masterKeyEncodingVersion :: String
  , masterKeyContent :: HexString
}
type RegisterUserRequest = {
    user :: UserCard
  , indexCardReference :: HexString
  , indexCardContent   :: HexString
  , cards :: Array (Tuple HexString HexString)
}

registerUser :: RegisterUserRequest -> Aff (Either ProtocolError HexString)
registerUser request = do
  let url = joinWith "/" [baseUrl, "users", show request.user.c]
  let body = (json $ encodeJson request) :: RequestBody
  registerUserResponse <- doGenericRequest url PUT [] (Just body) RF.string 
  pure $ case registerUserResponse of
    Left  error    -> Left error
    Right response -> if isStatusCodeOk response.status
                      then Right $ hex response.body
                      else Left  (ResponseError (unwrap response.status))
    
-- ----------------------------------------------------------------------------

sessionKeyHeaderName :: String
sessionKeyHeaderName = "clipperz-UserSession-ID"

type LoginResult =  { indexReference :: CardReference
                    , sessionKey     :: HexString
                    }

login :: SRP.SRPConf -> LoginForm.LoginForm -> Aff (Either ProtocolError LoginResult)
login srpConf formData = runExceptT $ do
  sessionKey :: HexString   <- ExceptT $ (fromArrayBuffer >>> Right) <$> SRP.randomArrayBuffer 32
  c          :: HexString   <- ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareC srpConf formData.username formData.password
  p          :: ArrayBuffer <- ExceptT $ Right <$> SRP.prepareP srpConf formData.username formData.password
  -- loginStep1Result <- ExceptT $ loginStep1 srpConf { c, sessionKey }
  loginStep1Result <- loginStep1 srpConf { c, sessionKey }
  -- { m1, kk, m2, encIndexReference: indexReference } <- ExceptT $ loginStep2 srpConf $ merge loginStep1Result { c, p, sessionKey }   
  { m1, kk, m2, encIndexReference: indexReference } <- loginStep2 srpConf $ merge loginStep1Result { c, p, sessionKey }   
  check :: Boolean <- ExceptT $ Right <$> SRP.checkM2 SRP.baseConfiguration loginStep1Result.aa m1 kk (toArrayBuffer m2)
  except $ case check of
    true  -> Right { indexReference, sessionKey }
    false -> Left  (SRPError "Client M2 doesn't match with server M2")

type LoginStep1Data = { c :: HexString
                      , sessionKey :: HexString
                      }

type LoginStep1Response = { s  :: HexString
                          , bb :: HexString
                          }

type LoginStep1Result = { aa :: BigInt
                        , a  :: BigInt
                        , s  :: HexString
                        , bb :: BigInt
                        }

-- loginStep1 :: SRP.SRPConf -> LoginStep1Data -> Aff (Either ProtocolError LoginStep1Result)
loginStep1 :: SRP.SRPConf -> LoginStep1Data -> ExceptT ProtocolError Aff LoginStep1Result
-- loginStep1 :: SRP.SRPConf -> LoginStep1Data -> StateT AppState (ExceptT ProtocolError Aff) LoginStep1Result
-- loginStep1 srpConf { c: c, sessionKey: sessionKey } = runExceptT $ do
loginStep1 srpConf { c: c, sessionKey: sessionKey } = do
  (Tuple a aa) <- withExceptT (\err -> SRPError $ show err) (ExceptT $ SRP.prepareA srpConf)
  let url  = joinWith "/" [baseUrl, "login", "step1", show c] :: String
  let body = json $ encodeJson { c, aa: fromBigInt aa }  :: RequestBody
  step1Response <- ExceptT $ doGenericRequest url POST [RE.RequestHeader sessionKeyHeaderName (show sessionKey)] (Just body) RF.json
  responseBody :: LoginStep1Response <- except $ if isStatusCodeOk step1Response.status
                                                 then lmap (\err -> DecodeError $ show err) (decodeJson step1Response.body)
                                                 else Left (ResponseError (unwrap step1Response.status))
  bb :: BigInt <- except $ note (SRPError "Error in converting B from String to BigInt") (toBigInt responseBody.bb)
  except $ if bb == fromInt (0)
           then Left $ SRPError "Server returned B == 0"
           else Right { aa, a, s: responseBody.s, bb }

type LogintStep2Data = { aa :: BigInt
                       , bb :: BigInt
                       , a  :: BigInt
                       , s  :: HexString
                       , c  :: HexString
                       , p  :: ArrayBuffer
                       , sessionKey :: HexString
                       }

type LoginStep2Response = { m2 :: HexString
                          , encIndexReference :: HexString
                          }

type LoginStep2Result = { m1 :: ArrayBuffer
                        , kk :: ArrayBuffer
                        , m2 :: HexString
                        , encIndexReference :: HexString
                        }

-- loginStep2 :: SRP.SRPConf -> LogintStep2Data -> Aff (Either ProtocolError LoginStep2Result)
loginStep2 :: SRP.SRPConf -> LogintStep2Data -> ExceptT ProtocolError Aff LoginStep2Result
-- loginStep2 srpConf { aa, bb, a, s, c, p, sessionKey } = runExceptT $ do
loginStep2 srpConf { aa, bb, a, s, c, p, sessionKey } = do
  x  :: BigInt      <- ExceptT $ (\ab -> note (SRPError "Cannot convert x from ArrayBuffer to BigInt") (arrayBufferToBigInt ab)) <$> (srpConf.kdf (toArrayBuffer s) p)
  ss :: BigInt      <- withExceptT (\err -> SRPError $ show err) (ExceptT $ SRP.prepareSClient srpConf aa bb x a)
  kk :: ArrayBuffer <- ExceptT $ Right <$> (SRP.prepareK srpConf ss)
  m1 :: ArrayBuffer <- ExceptT $ Right <$> (SRP.prepareM1 srpConf c s aa bb kk)
  let url  = joinWith "/" [baseUrl, "login", "step2", show c] :: String
  let body = json $ encodeJson { m1: fromArrayBuffer m1 }  :: RequestBody
  step2Response     <- ExceptT $ doGenericRequest url POST [RE.RequestHeader sessionKeyHeaderName (show sessionKey)] (Just body) RF.json
  responseBody :: LoginStep2Response <- except $ if isStatusCodeOk step2Response.status
                                                 then lmap (\err -> DecodeError $ show err) (decodeJson step2Response.body)
                                                 else Left (ResponseError (unwrap step2Response.status))
  except $ Right { m1, kk, m2: responseBody.m2, encIndexReference: responseBody.encIndexReference}

-- ----------------------------------------------------------------------------

getBlob :: HexString ->  Aff (Either ProtocolError (AXW.Response ArrayBuffer))
getBlob hash = do
  let url = joinWith "/" [baseUrl, "blobs", show $ hash]
  doGenericRequest url GET [] Nothing RF.arrayBuffer

getDecryptedBlob :: forall a. DecodeJson a => HexString -> CryptoKey -> Aff (Either ProtocolError a)
getDecryptedBlob reference key = runExceptT $ do
  response <- ExceptT $ getBlob reference
  if isStatusCodeOk response.status
    then withExceptT (\e -> CryptoError $ show e) (ExceptT $ decryptJson key response.body)
    else except $ Left (ResponseError (unwrap response.status))

postBlob :: ArrayBuffer -> Aff (Either ProtocolError (AXW.Response String))
postBlob blob = do
  let url = joinWith "/" [baseUrl, "blobs"]
  let body = json $ encodeJson (fromArrayBuffer blob) :: RequestBody
  doGenericRequest url POST [] (Just body) RF.string