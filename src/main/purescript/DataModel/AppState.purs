module DataModel.AppState where

import Data.Eq (class Eq)
import Data.HexString (HexString)
import Data.Map.Internal (Map)
import Data.Maybe (Maybe)
import Data.PrettyShow (class PrettyShow)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Data.Unit (Unit, unit)
import DataModel.AsyncValue (AsyncValue)
import DataModel.CardVersions.Card (Card)
import DataModel.IndexVersions.Index (Index)
import DataModel.SRPVersions.SRP (HashFunction, SRPConf)
import DataModel.UserVersions.User (MasterKey, UserInfo, UserInfoReferences)
import Functions.Donations (DonationLevel)
import Functions.HashCash (TollChallenge)

type Url = String
type Path = String
type SessionKey = HexString

type BackendSessionState = {
  b  :: HexString
, aa :: HexString
, bb :: HexString
}

type TollManager = {
  toll             :: AsyncValue HexString
, currentChallenge :: Maybe TollChallenge
}

data Proxy = OnlineProxy Url TollManager (Maybe SessionKey)
           | StaticProxy (Maybe BackendSessionState)

data ProxyResponse a = ProxyResponse Proxy a

discardResult :: forall a. ProxyResponse a -> ProxyResponse Unit
discardResult (ProxyResponse proxy _) = ProxyResponse proxy unit

responseValue :: forall a. ProxyResponse a -> a
responseValue (ProxyResponse _ a) = a

-- ==================

type CardsCache = Map HexString Card

type AppState =
  { proxy :: Proxy
  , username :: Maybe String
  , password :: Maybe String
  , pinEncryptedPassword :: Maybe HexString
  , c :: Maybe HexString
  , p :: Maybe HexString
  , s :: Maybe HexString
  , srpConf :: SRPConf
  , hash :: HashFunction
  , cardsCache :: CardsCache
  , masterKey :: Maybe MasterKey
  , userInfoReferences :: Maybe UserInfoReferences
  , userInfo :: Maybe UserInfo
  , donationLevel :: Maybe DonationLevel
  , index :: Maybe Index
  }

data AppStateResponse a = AppStateResponse AppState a

data InvalidStateError = CorruptedState String | MissingValue String | CorruptedSavedPassphrase String
instance showInvalidStateError :: Show InvalidStateError where
  show (CorruptedState           s) = "Corrupted state: " <> s
  show (MissingValue             s) = "Missing value in state: " <> s
  show (CorruptedSavedPassphrase s) = "Corrupted passphrase in local storage: " <> s

derive instance eqInvalidStateError :: Eq InvalidStateError

instance prettyShowInvalidStateError :: PrettyShow InvalidStateError where
  prettyShow (CorruptedState           _) = "The application state is corrupted, please restart it."
  prettyShow (MissingValue             _) = "The application state is corrupted, please restart it."
  prettyShow (CorruptedSavedPassphrase _) = "Clipperz could not decrypt your credentials, please log in without using the device PIN."
