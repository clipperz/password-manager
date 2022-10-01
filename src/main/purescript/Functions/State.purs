module Functions.State where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad (class Monad)
import Control.Monad.Except.Trans (ExceptT, runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor (class Functor, (<$>))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState)
import DataModel.Proxy (Proxy(..))
import Effect (Effect)

baseUrl :: String 
baseUrl = "http://localhost:8090" --TODO: get from configuration file/build

computeInitialState :: Effect AppState
computeInitialState = pure { proxy: (OnlineProxy baseUrl), sessionKey: Nothing, toll: Nothing, c: Nothing, p: Nothing }
