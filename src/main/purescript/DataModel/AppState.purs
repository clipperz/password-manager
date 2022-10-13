module DataModel.AppState where

import Data.Map.Internal (Map)
import Data.Maybe (Maybe)
import Data.HexString (HexString)
import Data.PrettyShow (class PrettyShow, prettyShow)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.AsyncValue (AsyncValue)
import DataModel.Card (Card)
import DataModel.Proxy (Proxy)
import DataModel.Communication.ProtocolError (ProtocolError)

type AppState =
  { proxy :: Proxy
  , sessionKey :: Maybe HexString
  , toll :: AsyncValue HexString
  , c :: Maybe HexString
  , p :: Maybe HexString
  -- , baseConfiguration :: SRPConf --TODO
  , cardsCache :: Map HexString Card
  }

data InvalidStateError = CorruptedState String | MissingValue String
instance showInvalidStateError :: Show InvalidStateError where
  show (CorruptedState s) = "Corrupted state: " <> s
  show (MissingValue s) = "Missing value in state: " <> s

instance prettyShowInvalidStateError :: PrettyShow InvalidStateError where
  prettyShow (CorruptedState s) = "The application state is corrupted, please restart it."
  prettyShow (MissingValue s) = "The application state is corrupted, please restart it."

data AppError = InvalidStateError InvalidStateError | ProtocolError ProtocolError
instance showAppError :: Show AppError where
  show (InvalidStateError err) = "Invalid state Error: "  <> show err
  show (ProtocolError err)     = "Protocol Error: " <> show err

instance prettyShowAppError :: PrettyShow AppError where
  prettyShow (InvalidStateError err) = prettyShow err
  prettyShow (ProtocolError err)     = prettyShow err
