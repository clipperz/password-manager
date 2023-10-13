module OperationalWidgets.LoginWidget where

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Concur.Core (Widget)
import Concur.React (HTML)
import Data.Either (either)
import Data.Function (($))
import Data.Functor ((<$>), (<#>))
import Data.Unit (Unit, unit)
import DataModel.Credentials (Credentials)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff.Class (liftAff)
import Functions.Login (doLogin)
import Views.LoginFormView (loginFormView, LoginDataForm)

data LoginWidgetResults = Credentials Credentials | LoginDone | LoginFailed String

loginWidget :: WidgetState -> LoginDataForm -> Widget HTML Unit
loginWidget widgetState loginData = do
  res <- case widgetState of
    Default   -> Credentials <$> loginFormView Default loginData
    Loading   -> Credentials <$> loginFormView Loading loginData
                 <|>
                 (liftAff $ (runExceptT $ doLogin loginData) <#> (either LoginFailed (\_ -> LoginDone)))
    Error err -> Credentials <$> loginFormView (Error err) loginData
  case res of
    Credentials credentials -> loginWidget Loading credentials
    LoginFailed err         -> loginWidget (Error err) loginData
    LoginDone               -> pure unit
