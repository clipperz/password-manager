module Views.LoginFormView where

import Control.Applicative (pure)
import Control.Bind (bind)
import Concur.Core (Widget)
import Concur.Core.FRP (loopS, loopW, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (div, div', text, label, input)
import Concur.React.Props as Props
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&), not)
import DataModel.Credentials (Credentials)
import DataModel.WidgetState (WidgetState(..))

-- import Views.SimpleWebComponents (simpleButton, simpleUserSignal, simplePasswordSignal, loadingDiv, simpleTextInputWidget)
import Views.SimpleWebComponents (simpleButton, loadingDiv)

-- | The data of the login form
type LoginDataForm =  { username :: String
                      , password :: String
                      }

emptyForm :: LoginDataForm
emptyForm = { username: "", password: "" }
-- For testing purpose
-- emptyForm = {username: "joe", password: "clipperz"}

isFormValid :: LoginDataForm -> Boolean
isFormValid { username, password } = username /= "" && password /= ""

loginFormView :: WidgetState -> LoginDataForm -> Widget HTML Credentials
loginFormView state loginFormData = 
  case state of
    Default   -> div [] [              form loginFormData]
    Loading   -> div [] [loadingDiv,   form loginFormData]
    Error err -> div [] [errorDiv err, form loginFormData]
  
  where
    errorDiv :: forall a. String -> Widget HTML a
    errorDiv err = div' [text err]

    form :: LoginDataForm -> Widget HTML Credentials
    form formData = div [Props.className "form"] [ do
        signalResult <- demand $ do
          formValues <- loopS formData $ \{username: username, password: password} -> do
            -- username' <- simpleUserSignal username
            username' <- loopW username (\v -> div [] [
              label [Props.htmlFor "username"] [text "Username"],
              (Props.unsafeTargetValue) <$> input [
                Props._type "text"
              , Props._id "username"
              , Props.placeholder "username"
              , Props.value v
              , Props.disabled false
              , Props.onChange
              ]
            ])
            -- password' <- simplePasswordSignal password
            password' <- loopW password (\v -> div [] [
              label [Props.htmlFor "passphrase"] [text "Passphrase"],
              (Props.unsafeTargetValue) <$> input [
                Props._type "password"
              , Props._id "passphrase"
              , Props.placeholder "passphrase"
              , Props.value v
              , Props.disabled false
              , Props.onChange
              ]
            ])
            pure { username: username', password: password' }
          result <- fireOnce (submitButton formValues)
          pure result
        -- liftEffect $ log $ "signalResult " <> show signalResult
        pure signalResult
    ]

    submitButton :: LoginDataForm -> Widget HTML LoginDataForm
    submitButton f = simpleButton "login" (not (isFormValid f)) f
