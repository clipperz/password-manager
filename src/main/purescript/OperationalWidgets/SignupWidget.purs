module OperationalWidgets.SignupWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text, div)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Either (either)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString)
import Data.Semigroup ((<>))
import Data.Unit (Unit, unit)
import Data.Show (class Show, show)
import DataModel.Credentials (Credentials)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Signup (signupUser)
import Functions.Login (doLogin)
import Record (merge)
import OperationalWidgets.LoginWidget (LoginWidgetResults(..))
import Views.SignupFormView (signupFormView, SignupDataForm)

data SignupWidgetResults = SignupCredentials Credentials | SignupDone Credentials HexString | SignupFailed String
instance showSignupWidgetResults :: Show SignupWidgetResults where
  show (SignupCredentials c) = "Credentials " <> show c
  show (SignupDone c _) = "Signup done " <> show c
  show (SignupFailed s) = "Signup failed " <> s

--------------------------------

signupWidgetWithLogin :: WidgetState -> SignupDataForm -> Widget HTML Unit
signupWidgetWithLogin state form = do
  signupResult <- signupWidget state form
  let login = liftAff $ (either LoginFailed (\_ -> LoginDone) <$> (runExceptT $ doLogin signupResult)) 
  loginResult <- (Credentials <$> signupFormView Loading (merge signupResult form)) <|> login
  case loginResult of
    Credentials credentials -> signupWidgetWithLogin Default (merge credentials form)
    LoginDone -> pure unit
    LoginFailed err -> signupWidgetWithLogin (Error ("Automatic login failed: " <> err)) (merge signupResult form) -- TODO: show login form?

signupWidget :: WidgetState -> SignupDataForm -> Widget HTML Credentials
signupWidget widgetState signupFormData = do
  res <- case widgetState of
    Default -> SignupCredentials <$> signupFormView Default signupFormData
    Loading -> do
      let creds = { username: signupFormData.username, password: signupFormData.password }
      let signup = liftAff $ runExceptT $ signupUser creds 
      (SignupCredentials <$> signupFormView Loading signupFormData) <|> (either (SignupFailed <<< show) (\r -> SignupDone creds r) <$> signup)
    Error err -> SignupCredentials <$> div [] [text err, signupFormView (Error err) signupFormData]
  case res of
    SignupCredentials credentials -> signupWidget Loading (merge credentials signupFormData)
    SignupDone credentials _ -> pure credentials
    SignupFailed err -> signupWidget (Error err) signupFormData

