module Widgets.LoginForm where

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT)
import Concur.Core (Widget)
import Concur.Core.FRP (loopS, fireOnce, demand)
import Concur.React (HTML)
import Concur.React.DOM (form', div, text)
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
import Functions.Login (doLogin, doLogin')
import Functions.SRP as SRP

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

data LoginWidgetResults = Credentials Credentials | LoginDone IndexReference | LoginFailed String

loginWidget :: SRP.SRPConf -> WidgetState -> LoginForm -> Widget HTML IndexReference
loginWidget conf widgetState loginData = do
  _ <- log "loginWidget START"
  res <- case widgetState of
    Default -> Credentials <$> loginForm true loginData
    Loading -> do
      let login = liftAff $ (either LoginFailed LoginDone <$> (runExceptT $ doLogin' conf loginData)) 
      (Credentials <$> loginForm false loginData) <|> login
    Error err -> Credentials <$> div [] [text err, loginForm true loginData]
  case res of
    Credentials credentials -> loginWidget conf Loading credentials
    LoginDone index -> pure index
    LoginFailed err -> loginWidget conf (Error err) loginData

  where 
    loginForm :: Boolean -> LoginForm -> Widget HTML Credentials
    loginForm loading formData = div [] [
                  div [ (P.className (if loading then "Loading" else "")) ] [],
                  form' [
                    do
                      signalResult <- demand $ do
                        formValues <- loopS loginData $ \{username: username, password: password} -> do
                          username' <- simpleUserSignal username
                          password' <- simplePasswordSignal password
                          pure { username: username', password: password' }
                        result <- fireOnce (submitWidget formValues)
                        pure result
                      liftEffect $ log $ "signalResult " <> show signalResult
                      pure signalResult
                  ]
                ]

submitWidget :: LoginForm -> Widget HTML LoginForm
submitWidget f = simpleButton "Log In" (not (isFormValid f)) f
