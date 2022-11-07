module Views.LoginFormView where

import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)
import Control.Semigroupoid ((<<<))
import Concur.Core (Widget)
import Concur.Core.FRP (loopS, loopW, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (div, div', text, label, input, a, fieldset)
import Concur.React.Props as Props
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, except, withExceptT)
import Data.Either (Either(..))
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.HexString (hex, toArrayBuffer, toString, Base(..))
import Data.HeytingAlgebra ((&&), not)
import Data.Int (fromString)
import Data.Maybe (maybe, Maybe(..), fromMaybe)
import Data.PrettyShow (prettyShow)
import Data.Semigroup ((<>))
import Data.Semiring ((*), (+))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Credentials (Credentials)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception as EX
import Functions.EncodeDecode (decryptJson)
import Functions.JSState (getAppState)
import Functions.Pin (generateKeyFromPin, decryptPassphrase, makeKey, isPinValid)
import Functions.State (getHashFunctionFromAppState)
import Views.SimpleWebComponents (simpleButton, loadingDiv, simpleNumberInputWidget)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem, removeItem, Storage)

data PinViewResult = Pin Int | NormalLogin

-- | The data of the login form
type LoginForm =  { username :: String
                  , password :: String
                  }

emptyForm :: LoginForm
emptyForm = { username: "", password: "" }
-- For testing purpose
-- emptyForm = {username: "joe", password: "clipperz"}

isFormValid :: LoginForm -> Boolean
isFormValid { username, password } = username /= "" && password /= ""

loginFormView :: WidgetState -> LoginForm -> Widget HTML Credentials
loginFormView state loginFormData = do
  storage <- liftEffect $ window >>= localStorage
  maybeSavedUser <- liftEffect $ getItem (makeKey "user") storage
  maybeSavedPassphrase <- liftEffect $ getItem (makeKey "passphrase") storage
  case maybeSavedUser, maybeSavedPassphrase of
    (Just user), (Just passphrase) -> formPin user passphrase state storage
    _, _ -> formNoPassphrase state loginFormData
  
  where
    formNoPassphrase :: WidgetState -> LoginForm -> Widget HTML Credentials
    formNoPassphrase state formData = 
      case state of
        Default   -> div [] [              form formData]
        Loading   -> div [] [loadingDiv,   form formData]
        Error err -> div [] [errorDiv err, form formData]

    formPin :: String -> String -> WidgetState -> Storage -> Widget HTML Credentials
    formPin user encryptedPassphrase state storage = do
      maybePin <- case state of
        Default -> div [] [pinView]
        Loading -> div [] [pinView]
        Error err -> div [] [errorDiv err, pinView]
      case maybePin of
        NormalLogin -> formNoPassphrase state (emptyForm { username = user })
        Pin pin -> do
          ei :: Either AppError Credentials <- liftAff $ runExceptT $ decryptPassphrase pin user encryptedPassphrase
          case ei of
            Right f -> do
              liftEffect $ setItem (makeKey "failures") (show 0) storage
              pure f
            Left err -> do
              failures <- liftEffect $ getItem (makeKey "failures") storage
              let count = (((fromMaybe 0) <<< fromString <<< (fromMaybe "")) failures) + 1
              liftEffect $ setItem (makeKey "failures") (show count) storage
              formPin user encryptedPassphrase (Error "Wrong pin") storage

    errorDiv :: forall a. String -> Widget HTML a
    errorDiv err = div' [text err]

    form :: LoginForm -> Widget HTML Credentials
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

    pinView :: Widget HTML PinViewResult
    pinView = div [Props.className "form"] [
        Pin <$> do
          signalResult <- demand $ do
            pin <- loopW "" (simpleNumberInputWidget "pinField" (text "Login") "PIN")
            pure $ (fromString pin) >>= (\p -> if isPinValid p then Just p else Nothing) 
          pure signalResult
      , NormalLogin <$ a [Props.onClick] [text "Use credentials to login"]
      ] 

    submitButton :: LoginForm -> Widget HTML LoginForm
    submitButton f = simpleButton "login" (not (isFormValid f)) f
