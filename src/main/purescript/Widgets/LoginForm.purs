module Widgets.LoginForm where

import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (form')
import Data.Eq ((/=))
import Data.Function (($))
import Data.HeytingAlgebra ((&&), not)
import Data.Semigroup ((<>))
import Data.Show (show)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import Widgets.SimpleWebComponents (simpleButton, simpleUserSignal, simplePasswordSignal)

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

loginForm :: Widget HTML LoginForm
loginForm = form' [
    do
      signalResult <- demand $ do
        formValues <- loopS emptyForm $ \{username: username, password: password} -> do
          username' <- simpleUserSignal username
          password' <- simplePasswordSignal password
          pure { username: username', password: password' }
        result <- fireOnce (submitWidget formValues)
        pure result
      liftEffect $ log $ "signalResult " <> show signalResult
      pure signalResult
  ]

submitWidget :: LoginForm -> Widget HTML LoginForm
submitWidget f = simpleButton "Log In" (not (isFormValid f)) f