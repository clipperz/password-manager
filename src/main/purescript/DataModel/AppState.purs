module DataModel.AppState
  ( AppError(..)
  , AppState
  , HashState(..)
  , InvalidStateError(..)
  , KDFState(..)
  , ProxyConnectionStatus(..)
  , SRPInfo
  , UserConnectionStatus(..)
  )
  where

import Prelude

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.BigInt (BigInt)
import Data.Generic.Rep (class Generic)
import Data.HexString (HexString)
import Data.Map.Internal (Map)
import Data.Maybe (Maybe)
import Data.PrettyShow (class PrettyShow, prettyShow)
import DataModel.AsyncValue (AsyncValue)
import DataModel.Card (Card)
import DataModel.Communication.ProtocolError (ProtocolError)
import DataModel.Proxy (Proxy)
import DataModel.SRP (SRPGroup)
import DataModel.User (MasterKey, UserInfoReferences, UserPreferences)
import Functions.HashCash (TollChallenge)

data UserConnectionStatus = UserLoggedIn | UserAnonymous
data ProxyConnectionStatus = ProxyOnline | ProxyOffline

derive instance showProxyConnectionStatus :: Eq ProxyConnectionStatus

type AppState =
  { proxy :: Proxy
  , sessionKey :: Maybe HexString
  , toll :: AsyncValue HexString
  , currentChallenge :: Maybe TollChallenge
  , username :: Maybe String
  , password :: Maybe String
  , c :: Maybe HexString
  , p :: Maybe HexString
  , s :: Maybe HexString
  , srpInfo :: SRPInfo
  , hash :: HashState
  , cardsCache :: Map HexString Card
  , masterKey :: Maybe MasterKey
  , userInfoReferences :: Maybe UserInfoReferences
  , userPreferences :: Maybe UserPreferences
  }

type SRPInfo = { group :: SRPGroup, k :: BigInt, hash:: HashState, kdf :: KDFState }

data KDFState = ConcatKDF
instance showKDFState :: Show KDFState where
  show ConcatKDF = "ConcatKDF"
derive instance genericKDFState :: Generic KDFState _
instance encodeJsonKDFState :: EncodeJson KDFState where
  encodeJson a = genericEncodeJson a
instance decodeJsonKDFState :: DecodeJson KDFState where
  decodeJson a = genericDecodeJson a

data HashState = SHA256 | SHA1
instance showHashState :: Show HashState where
  show SHA256 = "SHA256"
  show SHA1   = "SHA1"
derive instance genericHashState :: Generic HashState _
instance encodeJsonHashState :: EncodeJson HashState where
  encodeJson a = genericEncodeJson a
instance decodeJsonHashState :: DecodeJson HashState where
  decodeJson a = genericDecodeJson a

data InvalidStateError = CorruptedState String | MissingValue String | CorruptedSavedPassphrase String
instance showInvalidStateError :: Show InvalidStateError where
  show (CorruptedState s) = "Corrupted state: " <> s
  show (MissingValue s) = "Missing value in state: " <> s
  show (CorruptedSavedPassphrase s) =" Corrupted passphrase in local storage: " <> s

derive instance eqInvalidStateError :: Eq InvalidStateError

instance prettyShowInvalidStateError :: PrettyShow InvalidStateError where
  prettyShow (CorruptedState _) = "The application state is corrupted, please restart it."
  prettyShow (MissingValue _) = "The application state is corrupted, please restart it."
  prettyShow (CorruptedSavedPassphrase _) = "Clipperz could not decrypt your credentials, please log in without using the device PIN."

data AppError = InvalidStateError InvalidStateError | ProtocolError ProtocolError | ImportError String | CannotInitState String | InvalidOperationError String | InvalidVersioning String String | UnhandledCondition String
instance showAppError :: Show AppError where
  show (InvalidStateError err)     = "Invalid state Error: "  <> show err
  show (ProtocolError err)         = "Protocol Error: " <> show err
  show (ImportError err)           = "Import Error: " <> err
  show (CannotInitState err)       = "Cannot init state: " <> err
  show (InvalidOperationError err) = "Invalid operation error: " <> err
  show (InvalidVersioning elem v)  = "Invalid Versioning [" <> v <> "] of " <> elem 
  show (UnhandledCondition err)    = "Unahandled Condition: " <> err

instance prettyShowAppError :: PrettyShow AppError where
  prettyShow (InvalidStateError err)     = prettyShow err
  prettyShow (ProtocolError err)         = prettyShow err
  prettyShow (ImportError err)           = "Your imported values are not in the right format! (" <> err <> ")" 
  prettyShow (CannotInitState _)         = "Cannot init state, please try to reload"
  prettyShow (InvalidOperationError _)   = "Invalid operation error, something was not programmed correctly."
  prettyShow (InvalidVersioning elem _)  = elem <> " is encoded in an unsupported version" 
  prettyShow (UnhandledCondition     _)  = "" -- TODO?

derive instance eqAppError :: Eq AppError
