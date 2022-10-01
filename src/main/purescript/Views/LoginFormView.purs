module Views.LoginFormView where

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT)
import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (form', div, div', text)
import Concur.React.Props as P
import Data.Either (either)
import Data.Eq ((/=))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra ((&&), not)
import Data.Semigroup ((<>))
import Data.Show (show)
import DataModel.Credentials (Credentials)
import DataModel.WidgetState (WidgetState(..))
import DataModel.Index (IndexReference)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Login (doLogin)
import Functions.SRP as SRP

import Views.SimpleWebComponents (simpleButton, simpleUserSignal, simplePasswordSignal)

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
loginFormView state formData = 
  case state of
    Default -> div' [form false formData]
    Loading -> div' [loadingDiv, form true formData]
    Error err -> div' [errorDiv err, form false formData]
  
  where
    errorDiv err = div' [text err]
    loadingDiv = div [ (P.className "Loading") ] [text "LOADING"]
    form disabled formData = div' [ do
                                      signalResult <- demand $ do
                                        formValues <- loopS formData $ \{username: username, password: password} -> do
                                          username' <- simpleUserSignal username
                                          password' <- simplePasswordSignal password
                                          pure { username: username', password: password' }
                                        result <- fireOnce (submitButton formValues)
                                        pure result
                                      liftEffect $ log $ "signalResult " <> show signalResult
                                      pure signalResult
                                  ]

    submitButton :: LoginForm -> Widget HTML LoginForm
    submitButton f = simpleButton "Log In" (not (isFormValid f)) f
