module OperationalWidgets.SignupWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (text, a, p, form', div, div', fieldset)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT)
import Control.Semigroupoid ((<<<))
import Data.Either (Either(..), either)
import Data.Eq ((==), (/=))
import Data.Foldable (all)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString)
import Data.HeytingAlgebra ((&&), not)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe)
import Data.Ord ((<=))
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Show (class Show, show)
import Data.String (length)
import DataModel.Credentials (Credentials)
import DataModel.Index (IndexReference)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Signup (signupUser)
import Functions.Login (doLogin')
import Functions.SRP as SRP
import Record (merge)
import OperationalWidgets.LoginWidget (LoginWidgetResults(..))
import Views.SimpleWebComponents (simpleButton, simpleUserSignal, simpleVerifiedPasswordSignal, checkboxesSignal, PasswordForm)
import Views.SignupFormView (signupFormView, SignupDataForm)

data SignupWidgetResults = SignupCredentials Credentials | SignupDone Credentials HexString | SignupFailed String
instance showSignupWidgetResults :: Show SignupWidgetResults where
  show (SignupCredentials c) = "Credentials " <> show c
  show (SignupDone c _) = "Signup done " <> show c
  show (SignupFailed s) = "Signup failed " <> s

--------------------------------

signupWidgetWithLogin :: SRP.SRPConf -> WidgetState -> SignupDataForm -> Widget HTML IndexReference
signupWidgetWithLogin conf state form = do
  signupResult <- signupWidget conf state form
  let login = liftAff $ (either LoginFailed LoginDone <$> (runExceptT $ doLogin' conf signupResult)) 
  loginResult <- (Credentials <$> signupFormView Loading (merge signupResult form)) <|> login
  case loginResult of
    Credentials credentials -> signupWidgetWithLogin conf Default (merge credentials form)
    LoginDone index -> pure index
    LoginFailed err -> signupWidgetWithLogin conf (Error ("Automatic login failed: " <> err)) (merge signupResult form) -- TODO: show login form?

signupWidget :: SRP.SRPConf -> WidgetState -> SignupDataForm -> Widget HTML Credentials
signupWidget conf widgetState signupFormData = do
  res <- case widgetState of
    Default -> SignupCredentials <$> signupFormView Default signupFormData
    Loading -> do
      let creds = { username: signupFormData.username, password: signupFormData.password }
      -- let res = runExceptT $ signupUser conf creds
      let signup = liftAff $ runExceptT $ signupUser conf creds 
      (SignupCredentials <$> signupFormView Loading signupFormData) <|> (either (SignupFailed <<< show) (\r -> SignupDone creds r) <$> signup)
    Error err -> SignupCredentials <$> div [] [text err, signupFormView (Error err) signupFormData]
  case res of
    SignupCredentials credentials -> signupWidget conf Loading (merge credentials signupFormData)
    SignupDone credentials _ -> pure credentials
    SignupFailed err -> signupWidget conf (Error err) signupFormData

