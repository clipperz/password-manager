module Views.LoginFormView where

import Concur.Core (Widget)
import Concur.Core.FRP (loopS, loopW, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, div_, form, form_, input, label, span, text)
import Concur.React.Props as Props
import Control.Alt ((<$), (<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Either (Either(..), either)
import Data.Eq ((/=), (==))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&), not)
import Data.Maybe (Maybe(..))
import Data.Ord ((<))
import Data.String (length)
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.WidgetState (LoginFormData, LoginType(..))
import Functions.Communication.OneTimeShare (PIN)

type Username = String

type PinCredentials = { pin :: Int, user :: Username, passphrase :: String }

-- | The data of the login form
type LoginDataForm =  { username :: String
                      , password :: String
                      }

data LoginPageEvent   = LoginEvent Credentials
                      | LoginPinEvent PIN
                      | GoToCredentialLoginEvent Username
                      | GoToSignupEvent Credentials

emptyLoginFormData :: LoginFormData
emptyLoginFormData = { credentials: emptyCredentials, pin: "", loginType: CredentialLogin }

emptyForm :: LoginDataForm
emptyForm = { username: "", password: "" }

isFormValid :: LoginDataForm -> Boolean
isFormValid { username, password } = username /= "" && password /= ""


loginPage :: LoginFormData -> Widget HTML LoginPageEvent
loginPage {credentials, pin, loginType} =
  case loginType of
    CredentialLogin -> either GoToSignupEvent LoginEvent <$> credentialLoginWidget credentials
    PinLogin        -> do
      form [Props.className "loginForm"] [
        div [Props.className "loginInputs"] [
          span [] [text "Enter your PIN"]
        , LoginPinEvent <$> pinLoginWidget (length pin < 5) pin
        , GoToCredentialLoginEvent credentials.username <$ a [Props.onClick] [text "Login with passphrase"]
        ] <|> ((GoToSignupEvent credentials) <$ button [Props.onClick] [text "sign up"])
          ]

credentialLoginWidget :: LoginDataForm -> Widget HTML (Either Credentials Credentials)
credentialLoginWidget formData = do
    signalResult <- demand $ form_ [Props.className "loginForm"] do
      formValues <- loopS formData $ \{username: username, password: password} -> div_ [Props.className "loginInputs"] do
        username' <- loopW username (\v -> label [] [
            span [Props.className "label"] [text "Username"]
          , (Props.unsafeTargetValue) <$> input [
              Props._type "text"
            , Props.placeholder "username"
            , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
            , Props.value v
            , Props.autoFocus true
            , Props.disabled false
            , Props.onChange
            ]
          ])
        password' <- loopW password (\v -> div [] [
          label [] [
            span [Props.className "label"] [text "Passphrase"]
          , (Props.unsafeTargetValue) <$> input [
              Props._type "password"
            , Props.placeholder "passphrase"
            , Props.value v
            , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
            , Props.disabled false
            , Props.onChange
            ]
          ]
        ])
        pure { username: username', password: password' }
      result <- do
                  fireOnce ( div [Props.className "loginButton"] [
                              button [(Right formValues) <$ Props.onClick, Props.className "login", Props.disabled (not (isFormValid formValues))] [span [] [text "login"]]
                             ] <|> 
                              button [(Left  formValues) <$ Props.onClick]                                                                         [text "sign up"]
                           )
      pure result
    pure signalResult

pinLoginWidget :: Boolean -> String -> Widget HTML PIN
pinLoginWidget active pin = do
  signalResult <- demand $ do
    pin' <- loopW pin (\v -> div [] [
      label [] [
        span [Props.className "label"] [text "Login"]
      , (Props.unsafeTargetValue) <$> input [
          Props._type "password"
        , Props.placeholder "PIN"
        , Props.value v
        , Props.disabled (not active)
        , Props.onChange
        , Props.pattern "[0-9]{5}"
        ]
      ]
    ])
    pure $ if (length pin' == 5) && active then Just pin' else Nothing
  pure signalResult
