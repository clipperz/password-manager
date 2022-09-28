module Functions.Login where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.State (StateT, modify_, get, runStateT, mapStateT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState)
import DataModel.Credentials (Credentials)
import DataModel.Index (IndexReference)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (login, login')
import Functions.JSState (updateAppState, getAppState)
import Functions.SRP as SRP
import Functions.State (makeStateT)

doLogin :: SRP.SRPConf -> Credentials -> StateT AppState (ExceptT String Aff) IndexReference
doLogin conf { username, password } = do
  c           <- makeStateT $ ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareC conf username password
  p           <- makeStateT $ ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareP conf username password
  modify_ (\currentState -> currentState { c = Just c, p = Just p })
  
  indexReference <- mapStateT (withExceptT (\_ -> "Login failed")) (login conf)
  pure $ indexReference

doLogin' :: SRP.SRPConf -> Credentials -> ExceptT String Aff IndexReference
doLogin' conf { username, password } = do
  currentState <- withExceptT (show) (ExceptT $ liftEffect getAppState)
  c            <- ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareC conf username password
  p            <- ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareP conf username password
  ExceptT $ Right <$> updateAppState (currentState { c = Just c, p = Just p })
  
  indexReference <- withExceptT (\_ -> "Login failed") (login' conf)
  pure $ indexReference
