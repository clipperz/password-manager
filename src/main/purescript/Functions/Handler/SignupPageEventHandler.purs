module Functions.Handler.SignupPageEventHandler where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT)
import Data.Function ((#), ($))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DataModel.FragmentState as Fragment
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import Functions.Communication.Signup (signupUser)
import Functions.Handler.GenericHandlerFunctions (OperationState, doNothing, handleOperationResult, runStep)
import Functions.Handler.LoginPageEventHandler (loginSteps)
import Views.AppView (Page(..), WidgetState(..))
import Views.LoginFormView (LoginFormData, LoginType(..), emptyLoginFormData)
import Views.OverlayView (OverlayColor(..), hiddenOverlayInfo, spinnerOverlay)
import Views.SignupFormView (SignupPageEvent(..), getSignupDataFromCredentials)

getLoginFormData :: StatelessAppState -> LoginFormData
getLoginFormData {username: Just username, pinEncryptedPassword: Just _} = emptyLoginFormData { credentials = {username, password: ""}, loginType = PinLogin }
getLoginFormData _ = emptyLoginFormData

handleSignupPageEvent :: SignupPageEvent -> StatelessAppState -> Fragment.FragmentState -> Widget HTML OperationState

handleSignupPageEvent (SignupEvent cred) state@{proxy, hash, srpConf} fragmentState = 
  do
    ProxyResponse newProxy signupResult <- runStep (signupUser proxy hash srpConf cred) (WidgetState (spinnerOverlay "registering" Black) initialPage)
    res                                 <- loginSteps cred (state {proxy = newProxy}) fragmentState initialPage signupResult
    pure res
  
  # runExceptT 
  >>= handleOperationResult state initialPage Black

  where
    initialPage = Signup $ getSignupDataFromCredentials cred


handleSignupPageEvent (GoToLoginEvent cred) state _ = doNothing $ Tuple state (WidgetState hiddenOverlayInfo (Login (getLoginFormData state) {credentials = cred}))
