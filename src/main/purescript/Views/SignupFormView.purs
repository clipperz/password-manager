module Views.SignupFormView where

import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, div', div_, form, span, text)
import Concur.React.Props as Props
import Control.Alt ((<$), (<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Either (Either(..))
import Data.Eq ((==), (/=))
import Data.Foldable (all)
import Data.Function (($))
import Data.HeytingAlgebra ((&&), not)
import Data.Map (Map, fromFoldable)
import Data.Tuple (Tuple(..))
import DataModel.Credentials (Credentials)
import DataModel.WidgetState (WidgetState(..))
import Functions.Password (standardPasswordStrengthFunction)
import Record (merge)
import Views.SimpleWebComponents (loadingDiv, simpleButton, simpleUserSignal, simpleVerifiedPasswordSignal, checkboxesSignal, PasswordForm)

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
                              , Tuple "not_recoverable"  false
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
  Tuple "terms_of_service" ((text "I agree to the ") <|> (a [Props.href "https://clipperz.is/terms_service/", Props.target "_blank"] [(text "terms of service")])),
  Tuple "not_recoverable"   (text "I understand Clipperz won't be able to recover a lost password")
]

--------------------------------

signupFormView :: WidgetState -> SignupDataForm -> Widget HTML (Either Credentials Credentials) -- TODO: return SignupDataForm to show the compiled formWidget in loading
signupFormView state formData = 
  case state of
    Default   -> div [] [              formWidget false]
    Loading   -> div [] [loadingDiv,   formWidget true ]
    Error err -> div [] [errorDiv err, formWidget false]

  where
    errorDiv err = div' [text err]
    formWidget disabled = form [Props.className "signupForm", (Props.disabled disabled)] [
      do
        signalResult <- demand $ do
          formValues :: SignupDataForm <- loopS formData $ \{username: username, password: password, verifyPassword: verifyPassword, checkboxes: checkboxMap} -> div_ [Props.className "signupInputs"] do
            username' :: String <- simpleUserSignal "username" username
            eitherPassword :: Either PasswordForm String <- simpleVerifiedPasswordSignal standardPasswordStrengthFunction $ Left {password: password, verifyPassword: verifyPassword}
            checkboxMap' :: Array (Tuple String Boolean) <- div_ [Props.className "checkboxes"] $ checkboxesSignal checkboxMap checkboxesLabels   
            case eitherPassword of
              Left  passwords -> pure $ merge passwords { username: username', checkboxes: checkboxMap'}
              Right s         -> pure { username: username', password: s, verifyPassword: s, checkboxes: checkboxMap' }
          result <- fireOnce (submitWidget formValues)
          pure result
        pure signalResult
    ]

submitWidget :: SignupDataForm -> Widget HTML (Either Credentials Credentials)
submitWidget f@{ username, password } = div [Props.className "signupButton"] [simpleButton "signup" "Sign up" (not (isFormValid f)) (Right { username, password })]
                                        <|>
                                        button [(Left { username, password }) <$ Props.onClick] [span [] [text "login"]]
