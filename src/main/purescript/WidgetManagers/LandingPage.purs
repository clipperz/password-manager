module WidgetManagers.LandingPage where

import Control.Applicative (pure)
import Control.Bind (bind)
import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (show)
import Effect.Aff.Class (liftAff)
import Record (merge)
import RestBackendCommunication (login)
import SRP as SRP
import WidgetManagers.LoginManager (loginManager, LoginManagerResult)
import WidgetManagers.SignupManager (signupManager)
import Widgets.SignupForm (SignupForm)
import Widgets.SimpleWebComponents(simpleButton)

data LandingPageAction = ViewSignup | ViewLogin | Signup SignupForm | Login LoginManagerResult

data LandingPageView = SignupView | LoginView

landingPage :: SRP.SRPConf -> Widget HTML LoginManagerResult
landingPage conf = (landingPageWithState LoginView conf)

landingPageWithState :: LandingPageView -> SRP.SRPConf -> Widget HTML LoginManagerResult
landingPageWithState view conf = do
  res :: LandingPageAction <- demand $ fireOnce $ case view of
    LoginView -> div [] [
      Login <$> loginManager conf
      , simpleButton "Go to sign up" false ViewSignup
    ]
    SignupView -> div [] [
      Signup <$> signupManager conf
      , simpleButton "Go to log in" false ViewLogin
    ]
  case res of
    ViewSignup -> landingPageWithState SignupView conf
    ViewLogin  -> landingPageWithState LoginView conf
    Login rs   -> pure rs
    Signup f   -> do
      result <- liftAff $ login conf f
      case result of
        Left err -> div [] [text (show err), landingPageWithState LoginView conf]
        Right rs -> pure $ merge rs f
