module Functions.Login where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import DataModel.Credentials (Credentials)
import DataModel.Index (IndexReference)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (login)
import Functions.JSState (updateAppState, getAppState)
import Functions.SRP as SRP

doLogin :: SRP.SRPConf -> Credentials -> ExceptT String Aff IndexReference
doLogin conf { username, password } = do
  currentState <- withExceptT (show) (ExceptT $ liftEffect getAppState)
  c            <- ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareC conf username password
  p            <- ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareP conf username password
  ExceptT $ Right <$> updateAppState (currentState { c = Just c, p = Just p })
  
  indexReference <- withExceptT (\_ -> "Login failed") (login conf)
  pure $ indexReference
