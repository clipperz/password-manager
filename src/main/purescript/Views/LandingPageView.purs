module Views.LandingPageView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div)
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Functor ((<$))
import Data.Unit (Unit, unit)
-- import DataModel.User (IndexReference)
import DataModel.WidgetState (WidgetState(..))
import Record (merge)
import Views.SimpleWebComponents (simpleButton)
import Views.LoginFormView (LoginForm)
import Views.SignupFormView (SignupDataForm, emptyDataForm)
import OperationalWidgets.LoginWidget (loginWidget)
import OperationalWidgets.SignupWidget (signupWidgetWithLogin)

data LandingPageView = SignupView WidgetState SignupDataForm | LoginView WidgetState LoginForm

data LandingWidgetAction = LandingPageView LandingPageView | Login

landingPageView :: LandingPageView -> Widget HTML Unit
landingPageView view = do
  result <- case view of
    LoginView state form ->  div [] [
                    Login <$ loginWidget state form
                  , simpleButton "Go to sign up" false (LandingPageView (SignupView Default (merge form emptyDataForm)))
                  ]
    SignupView state form -> div [] [
                    Login <$ signupWidgetWithLogin state form -- TODO
                  , simpleButton "Go to log in" false (LandingPageView (LoginView Default { username: form.username, password: form.password }))
                  ]
  case result of
    LandingPageView v -> landingPageView v
    Login             -> pure unit
