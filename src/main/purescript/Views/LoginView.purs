module Views.LoginView
  ( loginView
  , loginViewSignal
  )
  where

import Control.Applicative (pure)
import Control.Bind (bind)
import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopS, loopW, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (form, input, label, span, text)
import Concur.React.Props as Props
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&), not)
import Data.Maybe (Maybe)
import DataModel.Credentials (Credentials)
import Views.SimpleWebComponents (simpleButton)

type LoginDataForm =  { username :: String
                      , password :: String
                      }

emptyForm :: LoginDataForm
emptyForm = { username: "", password: "" }

loginView :: Credentials -> Widget HTML Credentials
loginView credentials = do
  formWidget credentials

  where 
    formWidget :: LoginDataForm -> Widget HTML Credentials
    formWidget formData = form [Props.className "form"] [ do
      demand $ formSignal formData
    ]

loginViewSignal :: Signal HTML (Maybe Credentials)
loginViewSignal = formSignal emptyForm

formSignal :: LoginDataForm -> Signal HTML (Maybe Credentials)
formSignal formData = do
  formValues <- loopS formData $ \{username: username, password: password} -> do
    username' <- loopW username (\v -> label [] [
        span [Props.className "label"] [text "User[1]name"]
      , (Props.unsafeTargetValue) <$> input [
          Props._type "text"
        , Props.placeholder "username"
        , Props.value v
        , Props.disabled false
        , Props.onChange
        ]
      ])
    password' <- loopW password (\v -> label [] [
      span [Props.className "label"] [text "Passphrase"]
      , (Props.unsafeTargetValue) <$> input [
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

  where
    submitButton :: LoginDataForm -> Widget HTML LoginDataForm
    submitButton f = simpleButton "login" "login" (not (isFormValid f)) f

    isFormValid :: LoginDataForm -> Boolean
    isFormValid { username, password } = username /= "" && password /= ""
