module WidgetManagers.LandingPage where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT)
import Control.Monad.State (StateT, modify_, get, runStateT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState)
import DataModel.Index (IndexReference)
import Effect.Aff.Class (liftAff)
import Functions.SRP as SRP
import Functions.State (makeStateT)
import WidgetManagers.LoginManager (loginManager, doLogin)
import WidgetManagers.SignupManager (signupManager)
import Widgets.SignupForm (SignupForm)
import Widgets.SimpleWebComponents(simpleButton)

import Data.Semigroup ((<>))
import Effect.Class.Console (log)

data LandingPageAction = ViewSignup | ViewLogin | Signup (Tuple SignupForm AppState) | Login (Tuple IndexReference AppState) 

data LandingPageView = SignupView | LoginView

landingPage :: SRP.SRPConf -> StateT AppState (Widget HTML) IndexReference
landingPage conf = landingPageWithState LoginView conf

landingPageWithState :: LandingPageView -> SRP.SRPConf -> StateT AppState (Widget HTML) IndexReference
landingPageWithState view conf = do
  currentState <- get
  res :: LandingPageAction <- makeStateT $ demand $ fireOnce $ case view of
    LoginView -> div [] [
      Login <$> runStateT (loginManager conf) currentState
      , simpleButton "Go to sign up" false ViewSignup
    ]
    SignupView -> div [] [
      Signup <$> runStateT (signupManager conf) currentState
      , simpleButton "Go to log in" false ViewLogin
    ]
  case res of
    ViewSignup -> landingPageWithState SignupView conf
    ViewLogin  -> landingPageWithState LoginView conf
    Login (Tuple indexReference newState) -> do
      _ <- log $ "newState (landingPage) " <> (show newState)
      modify_ (\_ -> newState)
      pure $ indexReference
    Signup (Tuple form newState) -> do
      result :: Either String (Tuple IndexReference AppState) <- liftAff $ runExceptT $ runStateT (doLogin conf form) newState
      case result of
        Left err -> div [] [text (show err), landingPageWithState LoginView conf]
        Right (Tuple indexReference updateState) -> do
          modify_ (\_ -> updateState)
          pure $ indexReference
