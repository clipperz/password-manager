module Functions.State where

import Control.Applicative (pure)
import Data.Map.Internal (empty)
import Data.Maybe (Maybe(..))
import DataModel.AppState (AppState)
import DataModel.Proxy (Proxy(..))
import Effect (Effect)

baseUrl :: String 
baseUrl = "http://localhost:8090" --TODO: get from configuration file/build

computeInitialState :: Effect AppState
computeInitialState = pure { proxy: (OnlineProxy baseUrl), sessionKey: Nothing, toll: Nothing, c: Nothing, p: Nothing, cardsCache: empty }
