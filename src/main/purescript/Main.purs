module Main where

import Concur.React.Run (runWidgetInDom)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Data.Function (($))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)

import SRP as SRP

-- import Widgets.LoginForm as Widgets.LoginForm
-- import Widgets.SignupForm as Widgets.SignupForm
-- import Widgets.PasswordGenerator as Widgets.PasswordGenerator
-- import Widgets.CardsIndex as Widgets.CardsIndex
-- import Widgets.Cards as Widgets.Cards
-- import Widgets.HomePage as Widgets.HomePage

-- import WidgetManagers.SignupManager as SignupManager
-- import WidgetManagers.LoginManager as LoginManager
import WidgetManagers.LandingPage as LandingPage

main :: Effect Unit
main = do
  runWidgetInDom "app" $ do
    -- result <- (Widgets.LoginForm.loginForm)
    -- result <- (Widgets.PasswordGenerator.passwordGenerator)
    -- result <- (Widgets.HomePage.homePage)
    -- result <- (Widgets.SignupForm.signupForm)
    -- result <- (SignupManager.signupManager SRP.baseConfiguration)
    -- result <- (LoginManager.loginManager SRP.baseConfiguration)
    result <- (LandingPage.landingPage SRP.baseConfiguration)
    liftEffect $ log $ show result
    pure result