module OperationalWidgets.LoginWidget where

import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (runExceptT)
import Concur.Core (Widget)
import Concur.React (HTML)
import Data.Either (either)
import Data.Function (($))
import Data.Functor ((<$>))
import DataModel.Credentials (Credentials)
import DataModel.WidgetState (WidgetState(..))
import DataModel.Index (IndexReference)
import Effect.Aff.Class (liftAff)
import Functions.Login (doLogin)
import Views.LoginFormView (loginFormView, LoginForm)

data LoginWidgetResults = Credentials Credentials | LoginDone IndexReference | LoginFailed String

loginWidget :: WidgetState -> LoginForm -> Widget HTML IndexReference
loginWidget widgetState loginData = do
  res <- case widgetState of
    Default -> Credentials <$> loginFormView Default loginData
    Loading -> do
      let login = liftAff $ (either LoginFailed LoginDone <$> (runExceptT $ doLogin loginData)) 
      (Credentials <$> loginFormView Loading loginData) <|> login
    Error err -> Credentials <$> loginFormView (Error err) loginData
  case res of
    Credentials credentials -> loginWidget Loading credentials
    LoginDone index -> pure index
    LoginFailed err -> loginWidget (Error err) loginData
