module DataModel.AppState where

import Data.Maybe (Maybe)
import Data.HexString (HexString)
import DataModel.Proxy (Proxy)

type AppState = {
  proxy :: Proxy,
  sessionKey :: Maybe (HexString),
  toll :: Maybe (HexString),
  c :: Maybe (HexString),
  p :: Maybe (HexString) -- TODO?
}
