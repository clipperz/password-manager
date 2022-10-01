module Views.SignupFormView where

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
import Functions.Login (doLogin)
import Functions.Password (standardPasswordStrengthFunction)
import Functions.SRP as SRP
import Record (merge)
import OperationalWidgets.LoginWidget (LoginWidgetResults(..))
import Views.SimpleWebComponents (simpleButton, simpleUserSignal, simpleVerifiedPasswordSignal, checkboxesSignal, PasswordForm)

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

--------------------------------

signupFormView :: WidgetState -> SignupDataForm -> Widget HTML Credentials -- TODO: return SignupDataForm to show the compiled form in loading
signupFormView state formData = 
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
