module DataModel.AppState where

import Data.Argonaut.Decode.Class (class DecodeJson)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.BigInt (BigInt)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Map.Internal (Map)
import Data.Maybe (Maybe)
import Data.HexString (HexString)
import Data.PrettyShow (class PrettyShow, prettyShow)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.AsyncValue (AsyncValue)
import DataModel.Card (Card)
import DataModel.Proxy (Proxy)
import DataModel.User (IndexReference)
import DataModel.Communication.ProtocolError (ProtocolError)
import DataModel.SRP(SRPGroup, group1024, k)
import Functions.HashCash (TollChallenge)

type AppState =
  { proxy :: Proxy
  , sessionKey :: Maybe HexString
  , toll :: AsyncValue HexString
  , currentChallenge :: Maybe TollChallenge
  , c :: Maybe HexString
  , p :: Maybe HexString
  , srpInfo :: SRPInfo
  , hash :: HashState
  , cardsCache :: Map HexString Card
  , indexReference :: Maybe IndexReference
  }

type SRPInfo = { group :: SRPGroup, k :: BigInt, kdf :: KDFState }
baseSRPInfo :: SRPInfo
baseSRPInfo = {
  group: group1024
, k: k
, kdf: ConcatKDF
}

data KDFState = ConcatKDF
derive instance genericKDFState :: Generic KDFState _
instance encodeJsonKDFState :: EncodeJson KDFState where
  encodeJson a = genericEncodeJson a
instance decodeJsonKDFState :: DecodeJson KDFState where
  decodeJson a = genericDecodeJson a

data HashState = SHA256 | SHA1
derive instance genericHashState :: Generic HashState _
instance encodeJsonhashState :: EncodeJson HashState where
  encodeJson a = genericEncodeJson a
instance decodeJsonHashState :: DecodeJson HashState where
  decodeJson a = genericDecodeJson a

data InvalidStateError = CorruptedState String | MissingValue String
instance showInvalidStateError :: Show InvalidStateError where
  show (CorruptedState s) = "Corrupted state: " <> s
  show (MissingValue s) = "Missing value in state: " <> s

derive instance eqInvalidStateError :: Eq InvalidStateError

instance prettyShowInvalidStateError :: PrettyShow InvalidStateError where
  prettyShow (CorruptedState _) = "The application state is corrupted, please restart it."
  prettyShow (MissingValue _) = "The application state is corrupted, please restart it."

data AppError = InvalidStateError InvalidStateError | ProtocolError ProtocolError | ImportError String
instance showAppError :: Show AppError where
  show (InvalidStateError err) = "Invalid state Error: "  <> show err
  show (ProtocolError err)     = "Protocol Error: " <> show err
  show (ImportError err)       = "Import Error: " <> err

instance prettyShowAppError :: PrettyShow AppError where
  prettyShow (InvalidStateError err) = prettyShow err
  prettyShow (ProtocolError err)     = prettyShow err
  prettyShow (ImportError err)       = "Your imported values are not in the right format! (" <> err <> ")" 

derive instance eqAppError :: Eq AppError
