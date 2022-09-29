module Widgets.SignupForm where

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
  loginResult <- (Credentials <$> signupForm Loading (merge signupResult form)) <|> login
  case loginResult of
    Credentials credentials -> signupWidgetWithLogin conf Default (merge credentials form)
    LoginDone index -> pure index
    LoginFailed err -> signupWidgetWithLogin conf (Error ("Automatic login failed: " <> err)) (merge signupResult form) -- TODO: show login form?

signupWidget :: SRP.SRPConf -> WidgetState -> SignupDataForm -> Widget HTML Credentials
signupWidget conf widgetState signupFormData = do
  res <- case widgetState of
    Default -> SignupCredentials <$> signupForm Default signupFormData
    Loading -> do
      let creds = { username: signupFormData.username, password: signupFormData.password }
      -- let res = runExceptT $ signupUser conf creds
      let signup = liftAff $ runExceptT $ signupUser conf creds 
      (SignupCredentials <$> signupForm Loading signupFormData) <|> (either (SignupFailed <<< show) (\r -> SignupDone creds r) <$> signup)
    Error err -> SignupCredentials <$> div [] [text err, signupForm (Error err) signupFormData]
  case res of
    SignupCredentials credentials -> signupWidget conf Loading (merge credentials signupFormData)
    SignupDone credentials _ -> pure credentials
    SignupFailed err -> signupWidget conf (Error err) signupFormData

signupForm :: WidgetState -> SignupDataForm -> Widget HTML Credentials -- TODO: return SignupDataForm to show the compiled form in loading
signupForm state formData = 
  case state of
    Default   -> div' [form false]
    Loading   -> div' [loadingDiv, form true]
    Error err -> div' [errorDiv err, form false]

  where 
    errorDiv err = div' [text err ]
    loadingDiv = div [ (Props.className "Loading") ] [text "LOADING"]
    form disabled = fieldset [(Props.disabled disabled)] [
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
