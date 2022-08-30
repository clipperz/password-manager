module DataModel.AppState where

import Data.Maybe (Maybe)
import Data.HexString (HexString)

type AppState = {
  -- proxy :: Proxy,
  sessionKey :: Maybe (HexString),
  toll :: Maybe (HexString),
  c :: Maybe (HexString),
  p :: Maybe (HexString) -- TODO?
}