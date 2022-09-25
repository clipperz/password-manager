module Widgets.LandingPageWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Semigroupoid ((<<<))
import Data.Functor ((<$>))
import DataModel.WidgetState (WidgetState(..))
import DataModel.Credentials (Credentials)
import Record (merge)
import Widgets.SimpleWebComponents (simpleButton)
import Widgets.LoginForm (loginWidget, LoginForm)
import Widgets.SignupForm (signupWidget, SignupDataForm, emptyDataForm)

data LandingPageAction = Signup Credentials | Login Credentials 

data LandingPageView = SignupView WidgetState SignupDataForm | LoginView WidgetState LoginForm

data LandingWidgetAction = LandingPageView LandingPageView | LandingPageAction LandingPageAction

landingWidget :: LandingPageView -> Widget HTML LandingPageAction
landingWidget view = do
  result <- case view of
    LoginView state form ->  div [] [
                    (LandingPageAction <<< Login) <$> loginWidget state form
                  , simpleButton "Go to sign up" false (LandingPageView (SignupView Default (merge form emptyDataForm)))
                  ]
    SignupView state form -> div [] [
                    -- Signup <$> runStateT (signupManager conf) currentState
                    (LandingPageAction <<< Signup) <$> signupWidget state form
                  , simpleButton "Go to log in" false (LandingPageView (LoginView Default { username: form.username, password: form.password }))
                  ]
  case result of
    LandingPageView view -> landingWidget view
    LandingPageAction action -> pure action