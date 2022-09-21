module WidgetManagers.LoginManager where

import Concur.Core (Widget)
import Concur.Core.FRP (demandLoop, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.State (StateT, modify_, get, runStateT, mapStateT)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState)
import DataModel.Index (IndexReference)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Communication.Login (login)
import Functions.State (makeStateT)
import SRP as SRP
import Widgets.LoginForm (loginForm, LoginForm)

import Data.Semigroup ((<>))
import Data.Show (show)
import Effect.Class.Console (log)

type LoginManagerResult = { c :: HexString, p :: HexString, indexReference :: IndexReference, sessionKey :: HexString }

loginManager :: SRP.SRPConf -> StateT AppState (Widget HTML) IndexReference
loginManager conf = do
  currentState <- get
  Tuple result newState <- makeStateT $ demandLoop "" (\s -> loopW (Left s) (\err -> do
    loginFormResult <- case err of
      Left string -> div [] [text $ string, loginForm]
      Right _     -> loginForm
    liftAff $ runExceptT $ runStateT (doLogin conf loginFormResult) currentState
  ))
  modify_ (\_ -> newState)
  _ <- log $ "newState (loginManager)" <> show newState
  pure result

doLogin :: SRP.SRPConf -> LoginForm -> StateT AppState (ExceptT String Aff) IndexReference
doLogin conf { username, password } = do
  c           <- makeStateT $ ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareC conf username password
  p           <- makeStateT $ ExceptT $ Right <$> fromArrayBuffer <$> SRP.prepareP conf username password
  modify_ (\currentState -> currentState { c = Just c, p = Just p })
  
  indexReference <- mapStateT (withExceptT (\_ -> "Login failed")) (login conf)
  pure $ indexReference
