module DataModel.StatelessAppState where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.HexString (HexString)
import Data.Map.Internal (Map)
import Data.Maybe (Maybe)
import Data.Show (class Show, show)
import Data.Unit (Unit, unit)
import DataModel.AsyncValue (AsyncValue)
import DataModel.Card (Card)
import DataModel.SRP (HashFunction, SRPConf)
import DataModel.User (MasterKey, UserInfoReferences, UserPreferences)
import Functions.HashCash (TollChallenge)

data UserConnectionStatus = UserLoggedIn | UserAnonymous
data ProxyConnectionStatus = ProxyOnline | ProxyOffline

derive instance showProxyConnectionStatus :: Eq ProxyConnectionStatus

-- ==================

type Url = String
type Path = String
type SessionKey = HexString

type BackendSessionRecord = {
  b  :: HexString
, aa :: HexString
, bb :: HexString
}
data BackendSessionState = BackendSessionState BackendSessionRecord

derive instance eqBackendSessionState :: Eq BackendSessionState
derive instance genericBackendSessionState :: Generic BackendSessionState _

instance encodeJsonBackendSessionState :: EncodeJson BackendSessionState where
  encodeJson a = genericEncodeJson a

instance decodeJsonBackendSessionState :: DecodeJson BackendSessionState where
  decodeJson a = genericDecodeJson a

instance showBackendSessionState :: Show BackendSessionState where
  show (BackendSessionState r) = show r

type TollManager = {
  toll :: AsyncValue HexString
, currentChallenge :: Maybe TollChallenge
}

data Proxy = OnlineProxy Url TollManager (Maybe SessionKey)
           | StaticProxy (Maybe BackendSessionState)

-- derive instance eqProxy :: Eq Proxy
derive instance genericProxy :: Generic Proxy _

instance encodeJsonProxy :: EncodeJson Proxy where
  encodeJson a = genericEncodeJson a

instance decodeJsonProxy :: DecodeJson Proxy where
  decodeJson a = genericDecodeJson a

data ProxyResponse a = ProxyResponse Proxy a

discardResult :: forall a. ProxyResponse a -> ProxyResponse Unit
discardResult (ProxyResponse proxy _) = ProxyResponse proxy unit

responseValue :: forall a. ProxyResponse a -> a
responseValue (ProxyResponse _ a) = a

-- ==================

type StatelessAppState =
  { proxy :: Proxy
  , username :: Maybe String
  , password :: Maybe String
  , pinEncryptedPassword :: Maybe HexString
  , c :: Maybe HexString
  , p :: Maybe HexString
  , s :: Maybe HexString
  , srpConf :: SRPConf
  , hash :: HashFunction
  , cardsCache :: Map HexString Card
  , masterKey :: Maybe MasterKey
  , userInfoReferences :: Maybe UserInfoReferences
  , userPreferences :: Maybe UserPreferences
  }

data AppStateResponse a = AppStateResponse StatelessAppState a