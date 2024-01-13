module OperationalWidgets.App ( app ) where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, span, text)
import Concur.React.Props as Props
import Control.Alternative ((*>))
import Control.Bind (bind, (=<<))
import Data.Argonaut.Core (stringify)
import Data.Codec.Argonaut as CA
import Data.Eq ((==))
import Data.Function (($))
import Data.Monoid ((<>))
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppState)
import DataModel.Codec (widgetStateCodec)
import DataModel.FragmentState as Fragment
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.EnvironmentalVariables (currentCommit)
import Functions.Handler.CardManagerEventHandler (handleCardManagerEvent)
import Functions.Handler.GenericHandlerFunctions (OperationState)
import Functions.Handler.LoginPageEventHandler (handleLoginPageEvent)
import Functions.Handler.SignupPageEventHandler (getLoginFormData, handleSignupPageEvent)
import Functions.Handler.UserAreaEventHandler (handleUserAreaEvent)
import Views.AppView (Page(..), PageEvent(..), WidgetState(..), appView)
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

      resultEvent <- appView widgetState <> debugState widgetState
        
      appLoop =<< executeOperation resultEvent state fragmentState

executeOperation :: PageEvent -> AppState -> Fragment.FragmentState -> Widget HTML OperationState
executeOperation (SignupPageEvent          event)      = handleSignupPageEvent  event
executeOperation (LoginPageEvent           event)      = handleLoginPageEvent   event
executeOperation (MainPageCardManagerEvent event s)    = handleCardManagerEvent event s
executeOperation (MainPageUserAreaEvent    event s s') = handleUserAreaEvent    event s s'

debugState :: forall a. WidgetState -> Widget HTML a
debugState widgetState = do
  commit <- liftEffect $ currentCommit
  let jsonEncodedState = stringify $ CA.encode widgetStateCodec widgetState
  _ <-  if   commit == "development" 
        then button [Props._id "DEBUG", Props.onClick] [span [] [text "DEBUG"]] *> (liftAff $ copyToClipboard jsonEncodedState)
        else emptyEl
  debugState widgetState

emptyEl :: forall a. Widget HTML a
emptyEl = text ""