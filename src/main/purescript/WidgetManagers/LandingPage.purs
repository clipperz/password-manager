module WidgetManagers.LandingPage where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.State (StateT, modify_, get, evalStateT, runStateT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (fromArrayBuffer)
import Data.Maybe (Maybe(..))
import Data.Show (show)
import Data.Tuple (fst)
import DataModel.AppState (AppState)
import DataModel.Index (IndexReference)
import Effect.Aff.Class (liftAff)
import Functions.Communication.Login (login)
import Functions.State (makeStateT)
import SRP as SRP
import WidgetManagers.LoginManager (loginManager)
import WidgetManagers.SignupManager (signupManager)
import Widgets.SignupForm (SignupForm)
import Widgets.SimpleWebComponents(simpleButton)

data LandingPageAction = ViewSignup | ViewLogin | Signup SignupForm | Login IndexReference

data LandingPageView = SignupView | LoginView

landingPage :: SRP.SRPConf -> StateT AppState (Widget HTML) IndexReference
landingPage conf = landingPageWithState LoginView conf

landingPageWithState :: LandingPageView -> SRP.SRPConf -> StateT AppState (Widget HTML) IndexReference
landingPageWithState view conf = do
  currentState <- get
  res :: LandingPageAction <- makeStateT $ demand $ fireOnce $ case view of
    LoginView -> div [] [
      Login <$> fst <$> runStateT (loginManager conf) currentState
      , simpleButton "Go to sign up" false ViewSignup
    ]
    SignupView -> div [] [
      Signup <$> fst <$> runStateT (signupManager conf) currentState
      , simpleButton "Go to log in" false ViewLogin
    ]
  case res of
    ViewSignup -> landingPageWithState SignupView conf
    ViewLogin  -> landingPageWithState LoginView conf
    Login indexReference -> makeStateT $ pure $ indexReference
    Signup f@{ username, password } -> do
      result <- makeStateT $ liftAff $ login conf f
      case result of
        Left err -> div [] [text (show err), landingPageWithState LoginView conf]
        Right { indexReference, sessionKey } -> do
          c <- makeStateT $ liftAff $ fromArrayBuffer <$> SRP.prepareC conf username password
          p <- makeStateT $ liftAff $ fromArrayBuffer <$> SRP.prepareP conf username password
          modify_ (\state -> state { p = Just p, c = Just c, sessionKey = Just sessionKey })
          pure $ indexReference
