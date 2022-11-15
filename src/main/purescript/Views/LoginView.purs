module Views.LoginView
  ( loginView
  , loginViewSignal
  )
  where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, loopS, loopW, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (div, div', text, form, label, input, a)
import Concur.React.Props as Props

import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>), (<$), void)
import Data.HeytingAlgebra ((&&), not)
import Data.Maybe (Maybe)

import DataModel.Credentials (Credentials)
import Views.SimpleWebComponents (simpleButton, loadingDiv, simpleNumberInputWidget)

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
    -- username' <- simpleUserSignal username
    username' <- loopW username (\v -> div [] [
        label [Props.htmlFor "username"] [text "Username"]
      , (Props.unsafeTargetValue) <$> input [
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
        label [Props.htmlFor "passphrase"] [text "Passphrase"]
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
  -- liftEffect $ log $ "signalResult " <> show signalResult

  where
    submitButton :: LoginDataForm -> Widget HTML LoginDataForm
    submitButton f = simpleButton "login" (not (isFormValid f)) f

    isFormValid :: LoginDataForm -> Boolean
    isFormValid { username, password } = username /= "" && password /= ""
