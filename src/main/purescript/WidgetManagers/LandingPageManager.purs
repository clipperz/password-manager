module WidgetManagers.LandingPageManager where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except.Trans (runExceptT, withExceptT, ExceptT(..))
import Control.Monad.State (StateT, modify_, get, runStateT, mapStateT)
import Control.Semigroupoid ((>>>))
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppState)
import DataModel.Credentials (Credentials)
import DataModel.Index (IndexReference)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Communication.Signup (registerUser)
import Functions.Login (doLogin)
import Functions.Signup (prepareSignupParameters)
import Functions.SRP as SRP
import Functions.State (makeStateT, mapExceptT)
import Record (merge)
import Widgets.LandingPageWidget (landingWidget, LandingPageAction(..), LandingPageView(..), LandingWidgetAction(..))
import Widgets.LoginForm (emptyForm)
import Widgets.SignupForm (emptyDataForm, SignupDataForm)
import Widgets.SimpleWebComponents (simpleButton)

import Data.Semigroup ((<>))
import Effect.Class.Console (log)

landingPage :: SRP.SRPConf -> Widget HTML IndexReference
landingPage conf = landingPageWithState (LoginView Default emptyForm) conf

landingPageWithState :: LandingPageView -> SRP.SRPConf -> Widget HTML IndexReference -- Unit
landingPageWithState view conf = do
  _ <- log "landingPageWithState START"
  result <- landingWidget conf view
  case result of
    Signup credentials -> landingPageWithState view conf
    Login index -> pure index

  -- case result of
  --   Signup credentials -> do
  --     signupResult <- mapStateT (mapExceptT currentState >>> liftAff) $ do
  --       signupParameters <- makeStateT $ withExceptT (\_ -> "Registration failed") (ExceptT $ prepareSignupParameters conf credentials)
  --       mapStateT (\e -> withExceptT (\_ -> "Registration failed") e) (registerUser signupParameters)
  --     case signupResult of
  --       Left err -> landingPageWithState (SignupView (Error err) (merge credentials emptyDataForm)) conf
  --       Right value -> executeLogin credentials currentState
  --   Login credentials -> executeLogin credentials currentState 
  -- where 
  --   executeLogin :: Credentials -> AppState -> StateT AppState (Widget HTML) IndexReference
  --   executeLogin credentials currentState = do
  --     loginResult <- mapStateT (mapExceptT currentState >>> liftAff) (doLogin conf credentials)
  --     case loginResult of
  --       Left err -> landingPageWithState (LoginView (Error err) credentials) conf
  --       Right value -> do
  --         -- modify_ (\cs -> cs { indexReference = Just cs })
  --         -- pure unit 
  --         pure value
