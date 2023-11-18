module OperationalWidgets.App where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, form, h1, h3, header, li, p, span, text, ul)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Alternative ((*>), (<*))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Category ((<<<))
import Control.Monad.Except.Trans (ExceptT(..), runExceptT)
import Data.CommutativeRing ((+))
import Data.Either (Either(..), either)
import Data.Function (flip, (#), ($))
import Data.Functor ((<$), (<$>))
import Data.Int (fromString, toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord ((<))
import Data.Show (class Show, show)
import Data.String (length)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError, UserConnectionStatus(..))
import DataModel.Card (Card)
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import DataModel.WidgetState as WS
import Effect.Aff (Aff, delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.Login (PrepareLoginResult, computeLoginResult, loginStep1, loginStep2, prepareLogin)
import Functions.Communication.Signup (signupUser)
import Functions.Communication.StatelessOneTimeShare (PIN)
import Functions.EnvironmentalVariables (currentCommit)
import Functions.Pin (decryptPassphraseWithPin, deleteCredentials, makeKey)
import OperationalWidgets.HomePageWidget (homePageWidget)
import Record (merge)
import Unsafe.Coerce (unsafeCoerce)
import Views.Components (footerComponent)
import Views.LoginFormView (credentialLoginWidget, pinLoginWidget)
import Views.OverlayView (OverlayInfo, OverlayStatus(..), overlay)
import Views.SignupFormView (SignupDataForm, emptyDataForm, signupFormView)
import Web.HTML (window)
import Web.HTML.Window (localStorage)
import Web.Storage.Storage (getItem, setItem)

-- ========================================================
--  manage application effects (state, local storage, api)
-- ========================================================

app :: forall a. StatelessAppState -> Fragment.FragmentState -> Widget HTML a
app appState fragmentState = case fragmentState of
    Fragment.Login cred   -> appWithInitialOperation appState (WidgetState { status: Spinner, message: "logging in" } (Login  emptyLoginFormData {credentials = cred})) (LoginOperation cred)
    Fragment.Registration -> appLoop          (Tuple appState (WidgetState { status: Hidden,  message: ""           } (Signup emptyDataForm)))
    _                     -> appLoop          (Tuple appState (WidgetState { status: Hidden,  message: ""           } (Login  $ getLoginFormData appState)))
  
  where
    appWithInitialOperation :: StatelessAppState -> WidgetState -> Operation -> Widget HTML a
    appWithInitialOperation state widgetState operation = do
      appLoop =<< executeOperation operation (Tuple state widgetState)

    appLoop :: (Tuple StatelessAppState WidgetState) -> Widget HTML a
    appLoop (Tuple state widgetState) = do

      resultEvent <- appView widgetState

      let operation      = mapPageEventToOperation resultEvent
      let newWidgetState = updateWidgetState widgetState resultEvent
        
      appLoop =<< executeOperation operation (Tuple state newWidgetState)

    extractCardFromFragment :: Fragment.FragmentState -> Maybe Card
    extractCardFromFragment (Fragment.AddCard card) = Just card
    extractCardFromFragment _                       = Nothing

mapPageEventToOperation :: PageEvent -> Operation
mapPageEventToOperation (SignupPageEvent (SignupEvent   cred)) = SignupOperation       cred
mapPageEventToOperation (LoginPageEvent  (LoginPinEvent pin )) = LoginWithPinOperation pin
mapPageEventToOperation (LoginPageEvent  (LoginEvent    cred)) = LoginOperation        cred
mapPageEventToOperation _                                      = DoNothing

updateWidgetState :: WidgetState -> PageEvent -> WidgetState
-- SignupPageEvent
updateWidgetState widgetState (SignupPageEvent (SignupEvent     cred))              = WidgetState { status: Spinner, message: "registering" } (Signup emptyDataForm) -- TODO
updateWidgetState widgetState (SignupPageEvent (GoToLoginEvent  cred))              = WidgetState { status: Hidden , message: ""            } (Login  emptyLoginFormData {credentials = cred}) -- TODO
-- LoginPageEvent
updateWidgetState widgetState (LoginPageEvent  (LoginEvent      cred))              = WidgetState { status: Spinner, message: "logging in"  } (Login  emptyLoginFormData {credentials = cred})
updateWidgetState widgetState (LoginPageEvent  (LoginPinEvent   pin ))              = WidgetState { status: Hidden , message: ""            } (Login  emptyLoginFormData {pin = pin, loginType = PinLogin})
updateWidgetState widgetState (LoginPageEvent  (GoToCredentialLoginEvent username)) = WidgetState { status: Hidden , message: ""            } (Login  emptyLoginFormData {credentials = {username, password: ""}})
updateWidgetState widgetState (LoginPageEvent  (GoToSignupEvent cred))              = WidgetState { status: Hidden , message: ""            } (Signup emptyDataForm {username = cred.username, password = cred.password})
-- MainPageEvent
updateWidgetState widgetState (MainPageEvent   (LogoutEvent))                       = WidgetState { status: Hidden , message: ""            } (Login  emptyLoginFormData)


data LoginPageEvent   = LoginEvent Credentials
                      | LoginPinEvent PIN
                      | GoToCredentialLoginEvent Username
                      | GoToSignupEvent Credentials
data SignupPageEvent  = SignupEvent Credentials
                      | GoToLoginEvent Credentials
data MainPageEvent    = LogoutEvent

data PageEvent        = LoginPageEvent  LoginPageEvent
                      | SignupPageEvent SignupPageEvent
                      | MainPageEvent   MainPageEvent

data Operation        = LoginOperation  Credentials
                      | LoginWithPinOperation PIN
                      | SignupOperation Credentials
                      | DoNothing

type OperationState = Tuple StatelessAppState WidgetState

executeOperation :: Operation -> OperationState -> Widget HTML OperationState

executeOperation (SignupOperation cred) operationState@(Tuple state@{proxy, hash, srpConf} widgetState) = 
  do
    ProxyResponse newProxy signupResult <- runStep (signupUser proxy hash srpConf cred) widgetState
    let newOperationState = Tuple (state {proxy = newProxy}) (WidgetState { status: Spinner, message: "loggin in" } (Login  emptyLoginFormData {credentials = cred}))
    res                                 <- loginSteps cred newOperationState signupResult
    -- _                                   <- ExceptT $ Right <$> delayOperation 500 (WidgetState { status: Done, message: "" } (Main))
    pure res
  
  # runExceptT 
  >>= handleOperationResult operationState

executeOperation (LoginOperation cred) operationState@(Tuple state@{proxy, srpConf} widgetState) = 
  do
    ProxyResponse proxy' prepareLoginResult <- runStep (prepareLogin proxy srpConf cred) widgetState
    res                                     <- loginSteps cred (Tuple (state {proxy = proxy'}) widgetState) prepareLoginResult
    -- _                                       <- ExceptT $ Right <$> delayOperation 500 (WidgetState { status: Done, message: "" } (Main))
    pure res
  
  # runExceptT 
  >>= handleOperationResult operationState

executeOperation (LoginWithPinOperation pin) (Tuple state@{proxy, hash, srpConf, username, pinEncryptedPassword} widgetState@(WidgetState _ page)) = do
  do
    cred                                    <- runStep (decryptPassphraseWithPin hash pin username pinEncryptedPassword)    (WidgetState {status: Spinner, message: "Decrypt with PIN"} page)
    ProxyResponse proxy' prepareLoginResult <- runStep (prepareLogin proxy srpConf cred) (WidgetState {status: Spinner, message: "Prepare login"}    page)
    res                                     <- loginSteps cred (Tuple (state {proxy = proxy'}) widgetState) prepareLoginResult
    -- _                                       <- ExceptT $ Right <$> delayOperation 500 (WidgetState { status: Done, message: "" } (Main))
    pure res
  
  # runExceptT 
  >>= handlePinResult (Tuple state widgetState)
  >>= (\(Tuple operationState either) -> handleOperationResult operationState either)

executeOperation DoNothing (Tuple state widgetState) = (pure $ Tuple state widgetState) <|> (unsafeCoerce unit <$ appView widgetState)


loginSteps :: Credentials -> OperationState -> PrepareLoginResult -> ExceptT AppError (Widget HTML) OperationState
loginSteps cred (Tuple state@{proxy, hash: hashFunc, srpConf} (WidgetState _ page)) prepareLoginResult = do

  ProxyResponse proxy'   loginStep1Result   <- runStep (loginStep1         proxy   hashFunc srpConf prepareLoginResult.c)                                       (WidgetState {status: Spinner, message: "SRP step 1"   } page)
  ProxyResponse proxy''  loginStep2Result   <- runStep (loginStep2         proxy'  hashFunc srpConf prepareLoginResult.c prepareLoginResult.p loginStep1Result) (WidgetState {status: Spinner, message: "SRP step 2"   } page)
  ProxyResponse proxy''' stateUpdate        <- runStep (computeLoginResult proxy'' hashFunc srpConf cred prepareLoginResult loginStep1Result loginStep2Result)  (WidgetState {status: Spinner, message: "Validate user"} page)

  pure $ Tuple (merge stateUpdate (state {proxy = proxy'''})) (WidgetState {status: Hidden, message: ""} (Main))

runStep :: forall a. ExceptT AppError Aff a -> WidgetState -> ExceptT AppError (Widget HTML) a
-- runStep step widgetState = ExceptT $ ((step # runExceptT # liftAff) <* (liftAff $ delay (Milliseconds 1000.0))) <|> (defaultView widgetState)
runStep step widgetState = ExceptT $ (step # runExceptT # liftAff) <|> (defaultView widgetState)

defaultView :: forall a. WidgetState -> Widget HTML a
defaultView widgetState = (unsafeCoerce unit <$ appView widgetState)

type MaxPinAttemptsReached = Boolean

handlePinResult :: OperationState -> Either AppError OperationState -> Widget HTML (Tuple OperationState (Either AppError OperationState))
handlePinResult opState@(Tuple state (WidgetState _ page)) either = do
  storage <- liftEffect $ window >>= localStorage
  operationState <- case either of
    Right _ ->
      ( do
          liftEffect $ setItem (makeKey "failures") (show 0) storage
          pure opState
      )
      <|>
      (defaultView (WidgetState {status: Spinner, message: "Reset PIN attempts"} page))
    Left  _ -> (do
      failures <- liftEffect $ getItem (makeKey "failures") storage
      let count = (((fromMaybe 0) <<< fromString <<< (fromMaybe "")) failures) + 1
      if count < 3 then do
        liftEffect $ setItem (makeKey "failures") (show count) storage
        pure $ Tuple state (WidgetState {status: Failed, message: "error"} (Login $ emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = PinLogin}))
      else do
        liftEffect $ deleteCredentials storage
        pure $ Tuple state (WidgetState {status: Failed, message: "error"} (Login $ emptyLoginFormData {credentials = emptyCredentials {username = fromMaybe "" state.username}, loginType = CredentialLogin}))
    ) <|> (defaultView (WidgetState {status: Spinner, message: "Compute PIN attempts"} page))

  pure $ Tuple operationState either

handleOperationResult :: OperationState -> Either AppError OperationState -> Widget HTML OperationState
handleOperationResult operationState = either
                                        (manageError operationState)
                                        (\res@(Tuple _ (WidgetState _ page)) -> delayOperation 500 (WidgetState { status: Done, message: "" } page) *> pure res)
                                                
  where
    manageError :: OperationState -> AppError -> Widget HTML OperationState
    manageError (Tuple state (WidgetState _ page)) error = 
      case error of
        -- _ -> ErrorPage
        _ -> do
          delayOperation 500 (WidgetState { status: Failed,  message: "error" } page)
          pure $ Tuple state (WidgetState { status: Hidden,  message: ""      } page)

delayOperation :: Int -> WidgetState -> Widget HTML Unit
delayOperation time widgetState = ((liftAff $ delay (Milliseconds $ toNumber time)) <|> (unit <$ appView widgetState))


-- ==================================================
--                  ui elements
-- ==================================================

type SharedCardReference = String
type SharedCardPassword  = String
type Username = String

type LoginFormData = 
  { credentials :: Credentials
  , pin :: PIN
  , loginType :: LoginType
  }

data LoginType = CredentialLogin | PinLogin

getLoginFormData :: StatelessAppState -> LoginFormData
getLoginFormData {username: Just username, pinEncryptedPassword: Just _} = emptyLoginFormData { credentials = {username, password: ""}, loginType = PinLogin }
getLoginFormData _ = emptyLoginFormData

emptyLoginFormData :: LoginFormData
emptyLoginFormData = { credentials: emptyCredentials, pin: "", loginType: CredentialLogin }



data Page = Loading (Maybe Page) | Login LoginFormData | Signup SignupDataForm | Share (Maybe SharedCardReference) | Main --Maybe Card

data WidgetState = WidgetState OverlayInfo Page

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
    , MainPageEvent <$> div [Props.classList (Just <$> ["page", "main", show $ location Main page])] [
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
          , GoToCredentialLoginEvent credentials.username <$ a [Props.onClick] [text "Use credentials to login"]
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
