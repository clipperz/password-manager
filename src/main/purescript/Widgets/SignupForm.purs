module Widgets.SignupForm where

import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (text, a, p)
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
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Widgets.Utilities (PasswordStrengthFunction, PasswordStrength(..))
import Widgets.SimpleWebComponents (simpleButton, simpleUserSignal, simpleVerifiedPasswordSignal, checkboxesSignal, PasswordForm)

-- | The data of the signup form
type SignupForm = { username :: String
            , password :: String
            }

type DataForm = { username       :: String
                , password       :: String
                , verifyPassword :: String
                , checkboxes     :: Array (Tuple String Boolean)
                }

emptyDataForm :: DataForm
emptyDataForm = { username: ""
                , password: ""
                , verifyPassword: ""
                , checkboxes: [ Tuple "terms_of_service" true
                              , Tuple "not_recoverable" true
                              ]
                }
-- For testing purpose
-- emptyDataForm = { username: "joe"
--                 , password: "clipperz"
--                 , verifyPassword: "clipperz"
--                 , checkboxes: [ Tuple "terms_of_service" true
--                               , Tuple "not_recoverable" true
--                               ]
--                 }

isFormValid :: DataForm -> Boolean
isFormValid {username: user, password: pswd, verifyPassword: vpswd, checkboxes: chckbx} = user /= "" && pswd /= "" && pswd == vpswd && all (\(Tuple _ value) -> value) chckbx

checkboxesLabels :: forall a. Map String (Widget HTML a) 
checkboxesLabels = fromFoldable [
  Tuple "terms_of_service" (p [] [(text "I agree to the "), a [Props.href "https://clipperz.is/terms_service/", Props.target "_blank"] [(text "terms of service")]]),
  Tuple "not_recoverable" (text "I understand Clipperz won't be able to recover a lost password")
]

standardPasswordStrengthFunction :: PasswordStrengthFunction
standardPasswordStrengthFunction s = if (length s) <= 4 then Weak else Strong

--------------------------------

signupForm :: Widget HTML SignupForm
signupForm = do
  signalResult <- demand $ do
    formValues :: DataForm <- loopS emptyDataForm $ \{username: username, password: password, verifyPassword: verifyPassword, checkboxes: checkboxMap} -> do
      username' :: String <- simpleUserSignal username
      eitherPassword :: Either PasswordForm String <- simpleVerifiedPasswordSignal standardPasswordStrengthFunction $ Left {password: password, verifyPassword: verifyPassword}
      checkboxMap' :: Array (Tuple String Boolean) <- checkboxesSignal checkboxMap checkboxesLabels   
      case eitherPassword of
        Left { password: p, verifyPassword: vp} -> pure { username: username', password: p, verifyPassword: vp, checkboxes: checkboxMap'}
        Right s                                 -> pure { username: username', password: s, verifyPassword: s, checkboxes: checkboxMap' }
    result :: Maybe SignupForm <- fireOnce (submitWidget formValues)
    pure result
  liftEffect $ log $ "signalResult " <> show signalResult
  pure signalResult

submitWidget :: DataForm -> Widget HTML SignupForm
submitWidget (f@{username: user, password: pswd, verifyPassword: _}) = simpleButton "Sign up" (not (isFormValid f)) { username: user, password: pswd }