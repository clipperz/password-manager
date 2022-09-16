module DataModel.AppState where

import Data.Maybe (Maybe)
import Data.HexString (HexString)
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import DataModel.Proxy (Proxy)
import DataModel.Communication.ProtocolError (ProtocolError)

type AppState =
  { proxy :: Proxy
  , sessionKey :: Maybe (HexString)
  , toll :: Maybe (HexString)
  , c :: Maybe (HexString)
  , p :: Maybe (HexString)
  -- , baseConfiguration :: SRPConf --TODO
  }

data AppError = InvalidStateError String | ProtocolError ProtocolError
instance showAppError :: Show AppError where
  show (InvalidStateError err) = "Invalid state  Error: "  <> err
  show (ProtocolError err)     = "Protocol Error: " <> show err
