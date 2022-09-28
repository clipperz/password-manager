module Widgets.LandingPageWidget where

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
import Widgets.SimpleWebComponents (simpleButton)
import Widgets.LoginForm (loginWidget, LoginForm)
import Widgets.SignupForm (signupWidget, SignupDataForm, emptyDataForm)
import Functions.SRP as SRP

import Effect.Class.Console (log)


data LandingPageAction = Signup Credentials | Login IndexReference 

data LandingPageView = SignupView WidgetState SignupDataForm | LoginView WidgetState LoginForm

data LandingWidgetAction = LandingPageView LandingPageView | LandingPageAction LandingPageAction

landingWidget :: SRP.SRPConf -> LandingPageView -> Widget HTML LandingPageAction
landingWidget conf view = do
  _ <- log "landingWidget START"
  result <- case view of
    LoginView state form ->  div [] [
                    (LandingPageAction <<< Login) <$> loginWidget conf state form
                  , simpleButton "Go to sign up" false (LandingPageView (SignupView Default (merge form emptyDataForm)))
                  ]
    SignupView state form -> div [] [
                    -- Signup <$> runStateT (signupManager conf) currentState
                    (LandingPageAction <<< Signup) <$> signupWidget state form
                  , simpleButton "Go to log in" false (LandingPageView (LoginView Default { username: form.username, password: form.password }))
                  ]
  case result of
    LandingPageView view -> landingWidget conf view
    LandingPageAction action -> pure action
