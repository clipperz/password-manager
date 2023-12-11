module Views.DeleteUserView where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, loopS, loopW, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (button, div, form, h1, input, label, span, text)
import Concur.React.Props as Props
import Control.Alt (($>))
import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Data.Eq ((==))
import Data.Function (($))
import Data.Functor ((<$))
import Data.HeytingAlgebra (not, (&&))
import Data.Maybe (Maybe(..))
import Data.Unit (Unit, unit)
import DataModel.Credentials (Credentials)
import Views.Components (ClassName(..), verySimpleInputWidget, InputType(..), Enabled(..), Placeholder(..), Label(..))

type DeleteFormValue = {username :: String, password :: String, notRecoverable :: Boolean}
emptyFormValues :: DeleteFormValue
emptyFormValues = {username: "", password: "", notRecoverable: false}

deleteUserView :: Credentials -> Widget HTML Unit
deleteUserView credentials = do
  loop emptyFormValues
  where
    loop :: DeleteFormValue -> Widget HTML Unit
    loop deleteFormValue = do
      formValue <- deleteFormView deleteFormValue (Enabled true)
      res       <- (deleteFormView formValue (Enabled false) $> false)
                    <|> 
                   confirmationWidget
                    
      if res
      then pure unit
      else loop formValue
    
    deleteFormView :: DeleteFormValue -> Enabled -> Widget HTML DeleteFormValue
    deleteFormView deleteFormValue (Enabled enabled) = div [Props._id "deleteUserArea"] [form [Props.disabled (not enabled)] [
      h1 [] [text "Delete account"]
    , demand $ do
        formValues' <- loopS deleteFormValue $ \{username, password, notRecoverable} -> do
          username' :: String  <- loopW username $ verySimpleInputWidget (InputType "text")     (ClassName "username") (Label "Username")   (Enabled true) (Placeholder "username")   (matchingValueClassName credentials.username)
          password' :: String  <- loopW password $ verySimpleInputWidget (InputType "password") (ClassName "password") (Label "Passphrase") (Enabled true) (Placeholder "passphrase") (matchingValueClassName credentials.password)
          checkbox' :: Boolean <- loopW notRecoverable  (\v ->  label [Props.className "warning"] [
                                                                  span [Props.className "label"] [text "All my data will be permanently deleted. I understand that this action cannot be undone or canceled."]
                                                                , input [
                                                                    Props._type "checkbox"
                                                                  , Props.checked notRecoverable
                                                                  , Props.onChange
                                                                  ] $> (not v)
                                                                ])
          pure { username: username', password: password', notRecoverable: checkbox' }
        fireOnce (button [Props.className "delete", Props.disabled (isSubmitDisabled formValues'), Props.onClick] [span [] [text "Delete account"]] $> formValues')
    ]]

    isSubmitDisabled :: DeleteFormValue -> Boolean
    isSubmitDisabled formValues = (not (formValues.username == credentials.username && formValues.password == credentials.password && formValues.notRecoverable))

    matchingValueClassName :: String -> String -> Maybe ClassName
    matchingValueClassName expectedValue value = if expectedValue == value then (Just $ ClassName "valid") else Nothing

    confirmationWidget :: Widget HTML Boolean
    confirmationWidget = div [(Props.className "disableOverlay")] [
      div [Props.className "mask", false <$ Props.onClick] []
    , div [Props.className "dialog"] [
        div [Props.className "message"] [text "Are you sure you want to delete your account? You won't be able to recover it."]
      , div [Props.className "answers"] [
          button [Props.className "confirm", Props.onClick] [span [] [text "Yes"]] $> true
        , button [Props.className "cancel",  Props.onClick] [span [] [text "No"]]  $> false
        ]
      ]
    ]
