module Views.LoginFormView where

import Control.Alternative ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)
import Control.Semigroupoid ((<<<))
import Concur.Core (Widget)
import Concur.Core.FRP (loopS, loopW, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (div, div', form, text, label, input, a, span)
import Concur.React.Props as Props
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>), (<$), void)
import Data.HeytingAlgebra ((&&), not)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Semiring ((+))
import Data.Show (show)
import DataModel.AppState (AppError(..))
import DataModel.Credentials (Credentials)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Pin (decryptPassphrase, makeKey, isPinValid)
import Views.SimpleWebComponents (loadingDiv, simpleButton)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem, Storage)

data PinViewResult = Pin Int | NormalLogin

type PinCredentials = { pin :: Int, user :: String, passphrase :: String }

-- | The data of the login form
type LoginDataForm =  { username :: String
                      , password :: String
                      }

emptyForm :: LoginDataForm
emptyForm = { username: "", password: "" }

isFormValid :: LoginDataForm -> Boolean
isFormValid { username, password } = username /= "" && password /= ""

loginFormView :: WidgetState -> LoginDataForm -> Widget HTML Credentials
loginFormView state loginFormData = do
  storage <- liftEffect $ window >>= localStorage
  maybeSavedUser <- liftEffect $ getItem (makeKey "user") storage
  maybeSavedPassphrase <- liftEffect $ getItem (makeKey "passphrase") storage
  case maybeSavedUser, maybeSavedPassphrase of
    (Just user), (Just passphrase) -> formPin user passphrase state storage
    _, _ -> formNoPassphrase state loginFormData
  
  where
    formNoPassphrase :: WidgetState -> LoginDataForm -> Widget HTML Credentials
    formNoPassphrase st formData = 
      case st of
        Default   -> div [] [              formWidget formData]
        Loading   -> div [] [loadingDiv,   formWidget formData]
        Error err -> div [] [errorDiv err, formWidget formData]

    formPin :: String -> String -> WidgetState -> Storage -> Widget HTML Credentials
    formPin user encryptedPassphrase st storage = do
      maybePin <- case st of
        Default   -> div [] [pinView true ""]
        Loading   -> div [] [pinView false ""]
        Error err -> div [] [errorDiv err, pinView true ""]
      case maybePin of
        NormalLogin -> formNoPassphrase state (emptyForm { username = user })
        Pin pin -> do
          ei :: Either AppError Credentials <- (Left (CannotInitState "ciao") <$ div [] [pinView false (show pin)]) <|> (liftAff $ runExceptT $ decryptPassphrase pin user encryptedPassphrase)
          case ei of
            Right f -> do
              (void $ div [] [pinView false (show pin)]) <|> (liftEffect $ setItem (makeKey "failures") (show 0) storage)
              pure f
            Left e -> do
              log $ show e
              failures <- liftEffect $ getItem (makeKey "failures") storage
              let count = (((fromMaybe 0) <<< fromString <<< (fromMaybe "")) failures) + 1
              liftEffect $ setItem (makeKey "failures") (show count) storage
              formPin user encryptedPassphrase (Error "Wrong pin") storage

    errorDiv :: forall a. String -> Widget HTML a
    errorDiv err = div' [text err]

    formWidget :: LoginDataForm -> Widget HTML Credentials
    formWidget formData = form [Props.className "form"] [ do
        signalResult <- demand $ do
          formValues <- loopS formData $ \{username: username, password: password} -> do
            username' <- loopW username (\v -> div [] [
              label [] [
                span [Props.className "label"] [text "User[3]name"]
              , (Props.unsafeTargetValue) <$> input [
                  Props._type "text"
                , Props.placeholder "username"
                , Props.value v
                , Props.disabled false
                , Props.onChange
                ]
              ]
            ])
            password' <- loopW password (\v -> div [] [
              label [] [
                span [Props.className "label"] [text "Passphrase"]
              , (Props.unsafeTargetValue) <$> input [
                  Props._type "password"
                , Props.placeholder "passphrase"
                , Props.value v
                , Props.disabled false
                , Props.onChange
                ]
              ]
            ])
            pure { username: username', password: password' }
          result <- fireOnce (submitButton formValues)
          pure result
        pure signalResult
    ]

    pinView :: Boolean -> String -> Widget HTML PinViewResult
    pinView active pl = form [Props.className "form"] [
        Pin <$> do
          signalResult <- demand $ do
            pin <- loopW (if active then pl else "00000") (\v -> div [] [ -- TODO: don't really understand why changing the placeholder here works
              label [] [
                span [Props.className "label"] [text "Login"]
              , (Props.unsafeTargetValue) <$> input [
                  Props._type "password"
                , Props.placeholder "PIN"
                , Props.value v
                , Props.disabled false
                , Props.onChange
                , Props.pattern "[0-9]{5}"
                ]
              ]
            ])
            pure $ (fromString pin) >>= (\p -> if (isPinValid p) && active then Just p else Nothing) 
          pure signalResult
      , NormalLogin <$ a [Props.onClick] [text "Use credentials to login"]
      ] 

    submitButton :: LoginDataForm -> Widget HTML LoginDataForm
    submitButton f = simpleButton "login" "login" (not (isFormValid f)) f

loginFormView' :: LoginDataForm -> Widget HTML (Either PinCredentials Credentials)
loginFormView' loginFormData = do
  storage <- liftEffect $ window >>= localStorage
  maybeSavedUser <- liftEffect $ getItem (makeKey "user") storage
  maybeSavedPassphrase <- liftEffect $ getItem (makeKey "passphrase") storage
  case maybeSavedUser, maybeSavedPassphrase of
    (Just user), (Just passphrase) -> formPin user passphrase storage
    _, _ -> Right <$> (formWidget loginFormData)
  
  where
    formPin :: String -> String -> Storage -> Widget HTML (Either PinCredentials Credentials)
    formPin user encryptedPassphrase _ = do
      maybePin <- pinView true ""
      case maybePin of
        NormalLogin -> Right <$> (formWidget (emptyForm { username = user }))
        Pin pin -> pure $ Left {pin, user, passphrase: encryptedPassphrase}

    formWidget :: LoginDataForm -> Widget HTML Credentials
    formWidget formData = form [Props.className "form"] [ do
        signalResult <- demand $ do
          formValues <- loopS formData $ \{username: username, password: password} -> do
            username' <- loopW username (\v -> label [] [
                span [Props.className "label"] [text "Username"]
              , (Props.unsafeTargetValue) <$> input [
                  Props._type "text"
                , Props.placeholder "username"
                , Props.autoComplete "off", Props.autoCorrect "off", Props.autoCapitalize "off", Props.spellCheck false
                , Props.value v
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
          result <- fireOnce (submitButton formValues)
          pure result
        -- liftEffect $ log $ "signalResult " <> show signalResult
        pure signalResult
    ]

    pinView :: Boolean -> String -> Widget HTML PinViewResult
    pinView active pl = form [Props.className "form"] [
        Pin <$> do
          signalResult <- demand $ do
            pin <- loopW (if active then pl else "00000") (\v -> div [] [ -- TODO: don't really understand why changing the placeholder here works
              label [] [
                span [Props.className "label"] [text "Login"]
              , (Props.unsafeTargetValue) <$> input [
                  Props._type "password"
                , Props.placeholder "PIN"
                , Props.value v
                , Props.disabled false
                , Props.onChange
                , Props.pattern "[0-9]{5}"
                ]
              ]
            ])
            pure $ (fromString pin) >>= (\p -> if (isPinValid p) && active then Just p else Nothing) 
          pure signalResult
      , NormalLogin <$ a [Props.onClick] [text "Use credentials to login"]
      ] 

    submitButton :: LoginDataForm -> Widget HTML LoginDataForm
    submitButton f = simpleButton "login" "login" (not (isFormValid f)) f
