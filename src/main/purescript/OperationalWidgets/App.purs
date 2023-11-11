module OperationalWidgets.App where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, form, h1, h3, header, li, p, span, text, ul)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (=<<))
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (Either(..), either)
import Data.Function ((#), ($))
import Data.Functor ((<$), (<$>))
import Data.Maybe (Maybe(..))
import Data.Ord ((<))
import Data.Show (class Show, show)
import Data.String (length)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import DataModel.AppState (UserConnectionStatus(..))
import DataModel.Card (Card)
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import DataModel.WidgetState as WS
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Signup (signupUser)
import Functions.Communication.StatelessOneTimeShare (PIN)
import Functions.EnvironmentalVariables (currentCommit)
import Functions.Login (doLogin)
import OperationalWidgets.HomePageWidget (homePageWidget)
import Record (merge)
import Unsafe.Coerce (unsafeCoerce)
import Views.Components (footerComponent)
import Views.LoginFormView (credentialLoginWidget, pinLoginWidget)
import Views.OverlayView (OverlayStatus(..), OverlayInfo, overlay)
import Views.SignupFormView (SignupDataForm, emptyDataForm, signupFormView)

-- ========================================================
--  manage application effects (state, local storage, api)
-- ========================================================

app :: forall a. StatelessAppState -> Fragment.FragmentState -> Widget HTML a
app appState fragmentState = case fragmentState of
    Fragment.Login cred   -> appWithInitialOperation appState (WidgetState { status: Spinner, message: "logging in" } (Login  emptyLoginFormData {credentials = cred})) (LoginOperation cred)
    Fragment.Registration -> appLoop          (Tuple appState (WidgetState { status: Hidden,  message: ""           } (Signup emptyDataForm)))
    _                     -> appLoop          (Tuple appState (WidgetState { status: Hidden,  message: ""           } (Login  emptyLoginFormData)))
  
  where

    -- appLoop :: forall a'. StatelessAppState -> Action -> CredentialFormData -> Widget HTML a'
    -- appLoop state action credentialsFormData = do
    --   log $ show action
    --   loginType <- liftEffect getLoginType

    --   AppStateResponse newState nextAction <- do
    --     appView action loginType credentialsFormData (extractCardFromFragment fragmentState) <#> AppStateResponse state
    --     <|>
    --     (liftAff $ doOp state action)
    --   appLoop newState nextAction $ case nextAction of
    --     ShowPage Main        -> emptyCredentialFormData
    --     DoLogin credentials' -> credentialsFormData { credentials = credentials' }
    --     DoLoginWithPin pin'  -> credentialsFormData { pin         = pin'         }
    --     ShowError      _   _ -> credentialsFormData { pin         = ""           }
    --     _                    -> credentialsFormData

    appWithInitialOperation :: StatelessAppState -> WidgetState -> Operation -> Widget HTML a
    appWithInitialOperation state widgetState operation = do
      appLoop =<< executeOperation state widgetState operation

    appLoop :: (Tuple StatelessAppState WidgetState) -> Widget HTML a
    appLoop (Tuple state widgetState) = do

      resultEvent <- appView widgetState

      let operation      = mapPageEventToOperation resultEvent
      let newWidgetState = updateWidgetState widgetState resultEvent
        
      appLoop =<< executeOperation state newWidgetState operation

      
      
      -- case resultEvent of
      --   UpdateWidgetStateEvent newState newWidgetState Nothing -> appLoop newState newWidgetState event
      --   newEvent                                               -> appLoop state    widgetState    newEvent

    extractCardFromFragment :: Fragment.FragmentState -> Maybe Card
    extractCardFromFragment (Fragment.AddCard card) = Just card
    extractCardFromFragment _                       = Nothing

mapPageEventToOperation :: PageEvent -> Operation
mapPageEventToOperation (SignupPageEvent (SignupEvent cred)) = SignupOperation cred
mapPageEventToOperation (LoginPageEvent  (LoginEvent  cred)) = LoginOperation  cred
mapPageEventToOperation _                                    = DoNothing --TODO

updateWidgetState :: WidgetState -> PageEvent -> WidgetState
updateWidgetState widgetState (SignupPageEvent (SignupEvent cred)) = WidgetState { status: Spinner, message: "registering" } (Signup emptyDataForm) -- TODO
updateWidgetState widgetState (LoginPageEvent  (LoginEvent  cred)) = WidgetState { status: Spinner, message: "loggin in"   } (Login  emptyLoginFormData {credentials = cred})
updateWidgetState widgetState _ = widgetState -- TODO


data LoginPageEvent   = LoginEvent Credentials
                      | LoginPinEvent PIN
                      | GoToLoginWithCredentialsEvent Username
                      | GoToSignupEvent Credentials
data SignupPageEvent  = SignupEvent Credentials
                      | GoToLoginEvent Credentials
data MainPageEvent    = LogoutEvent

data PageEvent        = LoginPageEvent  LoginPageEvent
                      | SignupPageEvent SignupPageEvent
                      | MainPageEvent   MainPageEvent
                      | SharePageEvent

data Operation   = LoginOperation  Credentials
                 | SignupOperation Credentials
                 | DoNothing

-- data AppEvent         = PageEvent      PageEvent
--                       | OperationEvent StatelessAppState WidgetState (Maybe Operation)
--                       | BootEvent


-- processEvent :: StatelessAppState -> WidgetState -> AppEvent 
-- processEvent state (OperationEvent state' widgetState (Just operation)) = executeOperation state' operation

executeOperation :: StatelessAppState -> WidgetState -> Operation -> Widget HTML (Tuple StatelessAppState WidgetState)
executeOperation  state@{proxy, hash, srpConf} widgetState (SignupOperation cred) = do
  res <-  (liftAff $ signupUser proxy hash srpConf cred # runExceptT) <|> (unsafeCoerce unit <$ appView widgetState)
  case res of
    Left  err -> pure $ Tuple state (WidgetState { status: Failed,  message: "error" } (Signup emptyDataForm))
    Right (ProxyResponse newProxy _) -> do
      let newWidgetState = WidgetState { status: Spinner, message: "loggin in" } (Login  emptyLoginFormData {credentials = cred})
      let newAppState    = state {proxy = newProxy}

      executeOperation newAppState newWidgetState (LoginOperation cred)

executeOperation state@{proxy, hash, srpConf} widgetState (LoginOperation  cred) = do
  res <-  (liftAff $ doLogin proxy hash srpConf cred # runExceptT) <|> (unsafeCoerce unit <$ appView widgetState)
  pure $ either (\err         -> Tuple state                     (WidgetState { status: Failed,  message: "error" } (Login emptyLoginFormData {credentials = cred})))
                (\stateUpdate -> Tuple (merge stateUpdate state) (WidgetState { status: Done,    message: "" }      (Main))                                         )
                res

executeOperation state widgetState DoNothing = pure $ (Tuple state widgetState) -- <|> appView widgetState



-- processEvent state@{hash} (DoLoginWithPin pin) = do
--   _   <- liftEffect $ window >>= localStorage
--   res <- runExceptT $ decryptPassphraseWithRemoval hash pin "user" "passphrase"
--   pure $ AppStateResponse state (either (ShowError Login) DoLogin res)
-- processEvent state (ShowSuccess nextPage)   = (AppStateResponse state $ ShowPage nextPage) <$ delay (Milliseconds 500.0)
-- processEvent state (ShowError   nextPage _) = (AppStateResponse state $ ShowPage nextPage) <$ delay (Milliseconds 500.0)
-- processEvent state a = AppStateResponse state a <$ never


-- updateWidgetState :: WidgetState -> { state :: StatelessAppState, event :: AppEvent, error :: Maybe AppError } -> AppEvent -- always UpdateWidgetStateEvent
-- updateWidgetState widgetState@(WidgetState overlayInfo page) {state, event, error} Nothing  =
--   UpdateWidgetStateEvent state $ case event of
--     (PageEvent (SignupPageEvent (SignupEvent cred))) -> WidgetState { status: Spinner, message: "loading" } Signup cred
--     (PageEvent (LoginPageEvent  (LoginEvent  cred))) -> WidgetState { status: Spinner, message: "loading" } Login  cred
    
    
--     (PageEvent (LoginPageEvent (LoginEvent cred))) -> UpdateWidgetStateEvent state widgetState 
--     _ -> UpdateWidgetStateEvent state widgetState --TODO

-- updateWidgetState (WidgetState overlayInfo page) (Tuple state event) (Just _) =
--   UpdateWidgetStateEvent state $ case event of
--     (PageEvent (SignupPageEvent (SignupEvent cred))) -> WidgetState { status: Failed,  message: "error" } (Signup cred)
--     (PageEvent (LoginPageEvent  (LoginEvent  cred))) -> WidgetState { status: Failed,  message: "error" } (Login  cred)
    
    
--     _ -> WidgetState { status: Failed,  message: "error" } (Login emptyCredentials) -- TODO
 
 
  -- case nextAction of
    --     ShowPage Main        -> emptyCredentialFormData
    --     DoLogin credentials' -> credentialsFormData { credentials = credentials' }
    --     DoLoginWithPin pin'  -> credentialsFormData { pin         = pin'         }
    --     ShowError      _   _ -> credentialsFormData { pin         = ""           }
    --     _                    -> credentialsFormData


-- overlayFromAction :: forall a. Action -> Widget HTML a
-- overlayFromAction (DoLogin _)        = overlay { status: Spinner, message: "loading" }
-- overlayFromAction (DoLoginWithPin _) = overlay { status: Spinner, message: "loading" }
-- overlayFromAction (DoSignup _)       = overlay { status: Spinner, message: "loading" }
-- overlayFromAction (ShowError _ err)  = overlay { status: Failed,  message: "error"   } <* (log $ show err)
-- overlayFromAction (ShowSuccess _)    = overlay { status: Done,    message: ""        }
-- overlayFromAction _                  = overlay { status: Hidden,  message: "loading" }

-- ================================================

    -- appLoop :: forall a'. StatelessAppState -> WidgetState -> AppEvent -> Widget HTML a'
    -- appLoop state widgetState event = do

    --   resultEvent <- do 
    --     appView action loginType widgetState (extractCardFromFragment fragmentState)
    --     <|>
    --     (liftAff $ processEvent state event <#> computeWidgetState widgetState)
    --   case resultEvent of
    --     UpdateWidgetStateEvent newState newWidgetState -> pure $ Tuple newState newWidgetState
    --     newEvent                                       -> appLoop state widgetState newEvent

-- =============
-- res <- widget
-- doSomethingOnRes res <|> (schifo <* widget disabled)

-- doSomethingOnRes res <|> (unsafeCoerce $ widget disabled) -> should work, dangerous?


-- =============


-- doOp :: StatelessAppState -> Action -> Aff (AppStateResponse WidgetState) 
-- doOp state@{proxy, hash, srpConf} (DoSignup cred) = do
--   let res = signupUser proxy hash srpConf cred
--   (either (\err -> AppStateResponse state (ShowError Signup err)) (\(ProxyResponse newProxy _) -> AppStateResponse (state {proxy = newProxy}) (DoLogin cred))) <$> (runExceptT res)
-- doOp state@{proxy, hash, srpConf} (DoLogin cred) = do
--   res <- runExceptT $ doLogin proxy hash srpConf cred
--   pure $ (either (\err -> AppStateResponse state (ShowError Login err)) (\stateUpdate -> AppStateResponse (merge stateUpdate state) (ShowSuccess Main)) res)
-- doOp state@{hash} (DoLoginWithPin pin) = do
--   _   <- liftEffect $ window >>= localStorage
--   res <- runExceptT $ decryptPassphraseWithRemoval hash pin "user" "passphrase"
--   pure $ AppStateResponse state (either (ShowError Login) DoLogin res)
-- doOp state (ShowSuccess nextPage)   = (AppStateResponse state $ ShowPage nextPage) <$ delay (Milliseconds 500.0)
-- doOp state (ShowError   nextPage _) = (AppStateResponse state $ ShowPage nextPage) <$ delay (Milliseconds 500.0)
-- doOp state a = AppStateResponse state a <$ never

-- ==================================================
--                  ui elements
-- ==================================================

type SharedCardReference = String
type SharedCardPassword  = String
type Username = String

type CredentialFormData = 
  { credentials :: Credentials
  , pin :: PIN
  }

emptyCredentialFormData :: CredentialFormData
emptyCredentialFormData = { credentials: emptyCredentials, pin: ""}

type LoginFormData = 
  { credentials :: Credentials
  , pin :: PIN
  , loginType :: LoginType
  }

emptyLoginFormData :: LoginFormData
emptyLoginFormData = { credentials: emptyCredentials, pin: "", loginType: CredentialLogin }


data LoginType = CredentialLogin | PinLogin

data Page = Loading (Maybe Page) | Login LoginFormData | Signup SignupDataForm | Share (Maybe SharedCardReference) | Main --Maybe Card

data WidgetState = WidgetState OverlayInfo Page

-- data Action = DoLogout
--             | ShowPage Page
--             | DoLogin Credentials
--             | DoLoginWithPin PIN
--             | DoSignup Credentials
--             | ShowSharedCard SharedCardReference SharedCardPassword
--             | ShowError Page AppError
--             | ShowSuccess Page

appView :: WidgetState -> Widget HTML PageEvent
appView (WidgetState overlayInfo page) =
  appPages
  <|>
  overlay overlayInfo

  where 
    appPages :: Widget HTML PageEvent
    appPages = div [Props.className "mainDiv"] [
      headerPage page (Loading Nothing) []
    , LoginPageEvent <$> headerPage page (Login emptyLoginFormData) [
        loginPage $ case page of
          Login loginFormData -> loginFormData 
          _                   -> emptyLoginFormData
      ]
    , SignupPageEvent <$> headerPage page (Signup emptyDataForm) [ do
        let credentials = case page of
                            Signup credentials -> credentials
                            _                  -> emptyDataForm
        
        ((SignupEvent <$> (signupFormView WS.Default credentials)) 
        <|>
        (GoToLoginEvent {username: credentials.username, password: credentials.password} <$ button [Props.onClick] [text "login"]))
      ]
    , SharePageEvent <$ div [Props.classList (Just <$> ["page", "share", show $ location (Share Nothing) page])] [
        div [Props.className "content"] [text "share"]
      ]
    , MainPageEvent <$> div [Props.classList (Just <$> ["page", "main", show $ location Main page])] [
        -- LogoutEvent <$ (homePageWidget (if (action == (ShowPage Main)) then UserLoggedIn else UserAnonymous) cardToAdd)
        LogoutEvent <$ (homePageWidget UserLoggedIn Nothing)--cardToAdd)
      ]
    ]

    loginPage :: LoginFormData -> Widget HTML LoginPageEvent
    loginPage {credentials, pin, loginType} =
      case loginType of
        CredentialLogin -> LoginEvent <$> credentialLoginWidget credentials
        PinLogin        -> do
          form [Props.className "form"] [
            LoginPinEvent <$> pinLoginWidget (length pin < 5) pin --TODO not always true
          , GoToLoginWithCredentialsEvent "username" <$ a [Props.onClick] [text "Use credentials to login"]
          ]
      <|>
      ((GoToSignupEvent credentials) <$ button [Props.onClick] [text "sign up"]) -- TODO: if login fails this is reset

data PagePosition = LeftPosition | CenterPosition | RightPosition
instance showPagePosition :: Show PagePosition where
  show LeftPosition   = "left"
  show CenterPosition = "center"
  show RightPosition  = "right"

location :: Page -> Page -> PagePosition
location referencePage currentPage = case referencePage, currentPage of
  Loading _,  Loading _ -> CenterPosition
  Login _,    Login _   -> CenterPosition
  Signup _,   Signup _  -> CenterPosition
  Share _,    Share _   -> CenterPosition
  Main,       Main      -> CenterPosition

  Loading _,  _         -> LeftPosition
  Signup _,   Login _   -> LeftPosition
  Login _,    Share _   -> LeftPosition
  Signup _,   Share _   -> LeftPosition
  _,          Main      -> LeftPosition
  _,          _         -> RightPosition

pageClassName :: Page -> String
pageClassName (Loading _) = "loading"
pageClassName (Login _)   = "login"
pageClassName (Signup _)  = "signup"
pageClassName (Share _)   = "share"
pageClassName Main        = "main"

-- actionPage :: Action -> Page
-- actionPage (ShowPage page)      = page
-- actionPage (DoLogin _)          = Login
-- actionPage (DoLoginWithPin _)   = Login
-- actionPage (DoSignup _)         = Signup
-- actionPage (ShowSharedCard r _) = Share (Just r)
-- actionPage (DoLogout)           = Login
-- actionPage (ShowError page _)   = page
-- actionPage (ShowSuccess page)   = page

headerPage :: forall a. Page -> Page -> Array (Widget HTML a) -> Widget HTML a
headerPage currentPage page innerContent = do
  commitHash <- liftEffect $ currentCommit
  div [Props.classList (Just <$> ["page", pageClassName page, show $ location page currentPage])] [
    div [Props.className "content"] [
      headerComponent
    , div [Props.className "body"] innerContent
    , otherComponent
    , footerComponent commitHash
    , shortcutsDiv
    ]
  ]

headerComponent :: forall a. Widget HTML a
headerComponent =
  header [] [
    h1 [] [text "clipperz"]
  , h3 [] [text "keep it to yourself"]
  ]

otherComponent :: forall a. Widget HTML a
otherComponent =
  div [(Props.className "other")] [
    div [(Props.className "links")] [
      ul [] [
        li [] [a [Props.href "https://clipperz.is/about/",          Props.target "_blank"] [text "About"]]
      , li [] [a [Props.href "https://clipperz.is/terms_service/",  Props.target "_blank"] [text "Terms of service"]]
      , li [] [a [Props.href "https://clipperz.is/privacy_policy/", Props.target "_blank"] [text "Privacy"]]
      ]
    ]
  ]

shortcutsDiv :: forall a. Widget HTML a
shortcutsDiv = div [Props._id "shortcutsHelp", Props.className "hidden"] [
  p [] [span [] [text "/"]], p [] [text "search"]
, p [] [span [] [text "*"]], p [] [text "reset search"]
, p [] [span [] [text "Enter, d, RightArrow"]], p [] [text "open card"]
, p [] [span [] [text "Escape, a, LeftArrow"]], p [] [text "close card"]
, p [] [span [] [text "w, UpArrow, s, DownArrow"]], p [] [text "Navigate between cards"]
, p [] [span [] [text "lock"]], p [] [text "Lock"]
]

instance showPage :: Show Page where
  show (Loading _)  = "Loading"
  show (Login _)    = "Login"
  show (Signup _)   = "Signup"
  show (Share _)    = "Share"
  show (Main)       = "Main"

-- derive instance eqPage :: Eq Page

-- instance showAction :: Show Action where
--   show (ShowPage page)      = "Show Page " <> show page
--   show (DoLogin _)          = "Do Login"
--   show (DoLoginWithPin _)   = "Do LoginWithPint"
--   show (DoSignup _)         = "Do Signup"
--   show (ShowSharedCard _ _) = "Show Shared Card"
--   show (ShowError page _)   = "Show Error " <> show page 
--   show (ShowSuccess page)   = "Show Success " <> show page 
--   show (DoLogout)           = "DoLogout"

-- derive instance eqAction :: Eq Action
