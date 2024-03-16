module OperationalWidgets.App ( app ) where

import Concur.Core (Widget)
import Concur.React (HTML)
import Control.Bind (bind, (=<<))
import Data.Function (($))
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState)
import DataModel.FragmentState as Fragment
import DataModel.WidgetState (Page(..), WidgetState(..))
import Functions.Handler.CardManagerEventHandler (handleCardManagerEvent)
import Functions.Handler.DonationEventHandler (handleDonationPageEvent)
import Functions.Handler.GenericHandlerFunctions (OperationState)
import Functions.Handler.LoginPageEventHandler (handleLoginPageEvent)
import Functions.Handler.SignupPageEventHandler (getLoginFormData, handleSignupPageEvent)
import Functions.Handler.UserAreaEventHandler (handleUserAreaEvent)
import Views.AppView (PageEvent(..), appView)
import Views.LoginFormView (LoginPageEvent(..))
import Views.OverlayView (hiddenOverlayInfo)
import Views.SignupFormView (emptyDataForm)

app :: forall a. AppState -> Fragment.FragmentState -> Widget HTML a
app appState fragmentState = case fragmentState of
    Fragment.Login cred   -> appWithInitialOperation appState (LoginPageEvent $ LoginEvent cred)
    Fragment.Registration -> appLoop          (Tuple appState (WidgetState hiddenOverlayInfo (Signup  emptyDataForm)))
    _                     -> appLoop          (Tuple appState (WidgetState hiddenOverlayInfo (Login $ getLoginFormData appState)))

  where
    appWithInitialOperation :: AppState -> PageEvent -> Widget HTML a
    appWithInitialOperation state event = do
      appLoop =<< executeOperation event state fragmentState

    appLoop :: (Tuple AppState WidgetState) -> Widget HTML a
    appLoop (Tuple state widgetState) = do

      resultEvent <- appView widgetState

      appLoop =<< executeOperation resultEvent state fragmentState

executeOperation :: PageEvent -> AppState -> Fragment.FragmentState -> Widget HTML OperationState
executeOperation (SignupPageEvent          event)      = handleSignupPageEvent   event
executeOperation (LoginPageEvent           event)      = handleLoginPageEvent    event
executeOperation (MainPageCardManagerEvent event s)    = handleCardManagerEvent  event s
executeOperation (MainPageUserAreaEvent    event s s') = handleUserAreaEvent     event s s'
executeOperation (DonationPageEvent        event)      = handleDonationPageEvent event