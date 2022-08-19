module WidgetManagers.LoginManager where

import Control.Applicative (pure)
import Control.Bind (bind)
import Concur.Core (Widget)
import Concur.Core.FRP (demandLoop, loopW)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Monad.Except.Trans (ExceptT(..), runExceptT, withExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.HexString (HexString)
import Effect.Aff.Class (liftAff)
import Record (merge)
import RestBackendCommunication
import SRP as SRP
import Widgets.LoginForm (loginForm)

type LoginManagerResult = { username :: String, password :: String, indexReference :: HexString, sessionKey :: HexString }

loginManager :: SRP.SRPConf -> Widget HTML LoginManagerResult
loginManager conf = demandLoop "" (\s -> loopW (Left s) (\err -> do
  loginFormResult <- case err of
    Left string -> div [] [text $ string, loginForm]
    Right _     -> loginForm
  liftAff $ runExceptT $ do
    loginResult <- withExceptT (\_ -> "Login failed") (ExceptT $ login conf loginFormResult)
    pure $ merge loginFormResult loginResult
))
