module Views.ChangePasswordView where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopS, loopW)
import Concur.React (HTML)
import Concur.React.DOM (button, div, form, h1, input, label, span, text)
import Concur.React.Props as Props
import Control.Alt (($>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Eq ((/=), (==))
import Data.Function (($))
import Data.HeytingAlgebra (not, (&&))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import DataModel.Credentials (Credentials)
import Views.Components (ClassName(..), verySimpleInputWidget, InputType(..), Enabled(..), Placeholder(..), Label(..), entropyMeter)

type ChangePasswordDataForm = { username       :: String
                              , oldPassword    :: String
                              , password       :: String
                              , verifyPassword :: String
                              , notRecoverable :: Boolean
                              }
emptyChangePasswordDataForm :: ChangePasswordDataForm
emptyChangePasswordDataForm = { username: "", oldPassword: "", password: "", verifyPassword: "", notRecoverable: false}

changePasswordView :: Credentials -> Widget HTML String
changePasswordView currentCredentials = div [Props._id "changePasswordArea"] [form [] [
  h1 [] [text "Change passphrase"]
, do
    signalResult <- demand $ do
      formValues :: ChangePasswordDataForm <- loopS emptyChangePasswordDataForm $ \{username, oldPassword, password, verifyPassword, notRecoverable} -> do
        username'       :: String   <-  loopW username       $  verySimpleInputWidget (InputType "text")      (ClassName "username")  (Label "Username")          (Enabled true)  (Placeholder "username")              (matchingValueClassName currentCredentials.username)
        oldPassword'    :: String   <-  loopW oldPassword    $  verySimpleInputWidget (InputType "password")  (ClassName "password")  (Label "Old passphrase")    (Enabled true)  (Placeholder "old passphrase")        (matchingValueClassName currentCredentials.password)
        password'       :: String   <-  loopW password       (\p -> (
                                                                verySimpleInputWidget (InputType "password")  (ClassName "password") (Label "New passphrase")    (Enabled true)  (Placeholder "new passphrase")         (\_ -> Just $ ClassName "valid") p
                                                                <>
                                                                entropyMeter p
                                                              ))
        verifyPassword' :: String   <-  loopW verifyPassword $ verySimpleInputWidget (InputType "password")  (ClassName "password")  (Label "Verify passphrase") (Enabled true)  (Placeholder "confirm new passphrase") (matchingValueClassName password')
        checkbox'       :: Boolean  <-  loopW notRecoverable  (\v -> label [Props.className "no_recovery"] [
                                                                span [Props.className "label"] [text "I understand Clipperz won't be able to recover a lost password"]
                                                              , input [
                                                                  Props._type "checkbox"
                                                                , Props.checked v
                                                                , Props.onChange
                                                                ] $> (not v)
                                                              ])       
        pure { username: username', oldPassword: oldPassword', password: password', verifyPassword:verifyPassword', notRecoverable: checkbox' }
      result :: Maybe String <- fireOnce (submitWidget formValues)
      pure result
    pure signalResult
]]

  where
    matchingValueClassName :: String -> String -> Maybe ClassName
    matchingValueClassName expectedValue value = if expectedValue == value then (Just $ ClassName "valid") else Nothing

    submitWidget :: ChangePasswordDataForm -> Widget HTML String
    submitWidget { username, oldPassword, password, verifyPassword, notRecoverable } = do
      button [Props.onClick $> password, Props.disabled (not isEnabled)] [
        span [] [text "Change passphrase"]
      ]

      where
        isEnabled = (  username    == currentCredentials.username 
                    && oldPassword == currentCredentials.password 
                    && password    /= ""
                    && password    == verifyPassword
                    && notRecoverable
                    )