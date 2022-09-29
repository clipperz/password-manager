module Widgets.SignupForm where

import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (text, a, p, form', div)
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
import Data.HeytingAlgebra ((&&), not)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe)
import Data.Ord ((<=))
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Show (show)
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
import Widgets.LoginForm (LoginWidgetResults(..))
import Widgets.Utilities (PasswordStrengthFunction, PasswordStrength(..))
import Widgets.SimpleWebComponents (simpleButton, simpleUserSignal, simpleVerifiedPasswordSignal, checkboxesSignal, PasswordForm)


type SignupDataForm = { username       :: String
                      , password       :: String
                      , verifyPassword :: String
                      , checkboxes     :: Array (Tuple String Boolean)
                      }

emptyDataForm :: SignupDataForm
emptyDataForm = { username: ""
                , password: ""
                , verifyPassword: ""
                , checkboxes: [ Tuple "terms_of_service" false
                              , Tuple "not_recoverable" false
                              ]
                }

isFormValid :: SignupDataForm -> Boolean
isFormValid { username, password, verifyPassword, checkboxes } =
      username /= ""
  &&  password /= ""
  &&  password == verifyPassword 
  &&  all (\(Tuple _ value) -> value) checkboxes

checkboxesLabels :: forall a. Map String (Widget HTML a) 
checkboxesLabels = fromFoldable [
  Tuple "terms_of_service" (p [] [(text "I agree to the "), a [Props.href "https://clipperz.is/terms_service/", Props.target "_blank"] [(text "terms of service")]]),
  Tuple "not_recoverable" (text "I understand Clipperz won't be able to recover a lost password")
]

standardPasswordStrengthFunction :: PasswordStrengthFunction
standardPasswordStrengthFunction s = if (length s) <= 4 then Weak else Strong

--------------------------------

data SignupWidgetResults = SignupCredentials Credentials | SignupDone Credentials | SignupFailed String

--------------------------------

signupWidgetWithLogin :: SRP.SRPConf -> WidgetState -> SignupDataForm -> Widget HTML IndexReference
signupWidgetWithLogin conf state form = do
  signupResult <- signupWidget conf state form
  let login = liftAff $ (either LoginFailed LoginDone <$> (runExceptT $ doLogin' conf signupResult)) 
  loginResult <- (Credentials <$> signupWidget conf Loading (merge signupResult form)) <|> login
  case loginResult of
    Credentials credentials -> signupWidgetWithLogin conf Loading (merge credentials form)
    LoginDone index -> pure index
    LoginFailed err -> signupWidgetWithLogin conf (Error ("Automatic login failed: " <> err)) (merge signupResult form) -- TODO: show login form?

signupWidget :: SRP.SRPConf -> WidgetState -> SignupDataForm -> Widget HTML Credentials
signupWidget conf widgetState signupFormData = do
  res <- case widgetState of
    Default -> SignupCredentials <$> signupForm false signupFormData
    Loading -> do
      let creds = { username: signupFormData.username, password: signupFormData.password }
      let signup = liftAff $ (either (SignupFailed <<< show) (\_ -> SignupDone creds) <$> (runExceptT $ signupUser conf creds)) 
      (SignupCredentials <$> signupForm true signupFormData) <|> signup
    Error err -> do
      _ <- log err
      SignupCredentials <$> div [] [text err, signupForm false signupFormData]
  case res of
    SignupCredentials credentials -> signupWidget conf Loading (merge credentials signupFormData)
    SignupDone credentials -> pure credentials
    SignupFailed err -> signupWidget conf (Error err) signupFormData

  where 
    signupForm :: Boolean -> SignupDataForm -> Widget HTML Credentials -- TODO: return SignupDataForm to show the compiled form in loading
    signupForm loading formData = div [] [
      div [ (Props.className (if loading then "Loading" else "")) ] (if loading then [text "LOADING"] else []),
      do
        signalResult <- demand $ do
          formValues :: SignupDataForm <- loopS formData $ \{username: username, password: password, verifyPassword: verifyPassword, checkboxes: checkboxMap} -> do
            username' :: String <- simpleUserSignal username
            eitherPassword :: Either PasswordForm String <- simpleVerifiedPasswordSignal standardPasswordStrengthFunction $ Left {password: password, verifyPassword: verifyPassword}
            checkboxMap' :: Array (Tuple String Boolean) <- checkboxesSignal checkboxMap checkboxesLabels   
            case eitherPassword of
              Left  passwords -> pure $ merge passwords { username: username', checkboxes: checkboxMap'}
              Right s         -> pure { username: username', password: s, verifyPassword: s, checkboxes: checkboxMap' }
          result :: Maybe Credentials <- fireOnce (submitWidget formValues)
          pure result
        liftEffect $ log $ "signalResult " <> show signalResult
        pure signalResult
    ]

submitWidget :: SignupDataForm -> Widget HTML Credentials
submitWidget f@{ username, password } = simpleButton "Sign up" (not (isFormValid f)) { username, password }
