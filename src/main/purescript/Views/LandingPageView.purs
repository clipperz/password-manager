module Views.LandingPageView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Semigroupoid ((<<<))
import Data.Functor ((<$>))
import DataModel.Credentials (Credentials)
import DataModel.Index (IndexReference)
import DataModel.WidgetState (WidgetState(..))
import Record (merge)
import Views.SimpleWebComponents (simpleButton)
import Views.LoginFormView (LoginForm)
import Views.SignupFormView (SignupDataForm, emptyDataForm)
import OperationalWidgets.LoginWidget (loginWidget)
import OperationalWidgets.SignupWidget (signupWidgetWithLogin)
import Functions.SRP as SRP

import Effect.Class.Console (log)

data LandingPageView = SignupView WidgetState SignupDataForm | LoginView WidgetState LoginForm

data LandingWidgetAction = LandingPageView LandingPageView | Login IndexReference

landingPageView :: SRP.SRPConf -> LandingPageView -> Widget HTML IndexReference
landingPageView conf view = do
  result <- case view of
    LoginView state form ->  div [] [
                    Login <$> loginWidget conf state form
                  , simpleButton "Go to sign up" false (LandingPageView (SignupView Default (merge form emptyDataForm)))
                  ]
    SignupView state form -> div [] [
                    Login <$> signupWidgetWithLogin conf state form -- TODO
                  , simpleButton "Go to log in" false (LandingPageView (LoginView Default { username: form.username, password: form.password }))
                  ]
  case result of
    LandingPageView view -> landingPageView conf view
    Login index          -> pure index
