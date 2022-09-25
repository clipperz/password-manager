module Widgets.SignupForm where

import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (text, a, p, form', div)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Either (Either(..))
import Data.Eq ((==), (/=))
import Data.Foldable (all)
import Data.Function (($))
import Data.HeytingAlgebra ((&&), not)
import Data.Map (Map, fromFoldable)
import Data.Maybe (Maybe)
import Data.Ord ((<=))
import Data.Semigroup ((<>))
import Data.Tuple (Tuple(..))
import Data.Show (show)
import Data.String (length)
import DataModel.Credentials (Credentials)
import DataModel.WidgetState (WidgetState(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Record (merge)
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

signupWidget :: WidgetState -> SignupDataForm -> Widget HTML Credentials
signupWidget widgetState signupFormData = do
  case widgetState of
    Default -> signupForm
    Loading -> do
      _ <- div [] [ text "loading"]
      pure { username: signupFormData.username, password: signupFormData.password }
    Error err -> div [] [text err, signupForm]

  where 
    signupForm = div [] [
      do
        signalResult <- demand $ do
          formValues :: SignupDataForm <- loopS signupFormData $ \{username: username, password: password, verifyPassword: verifyPassword, checkboxes: checkboxMap} -> do
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