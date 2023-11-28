module Views.AppView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, form, h1, h3, header, li, p, span, text, ul)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Bind (bind)
import Data.Either (either)
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.HeytingAlgebra (not)
import Data.Maybe (Maybe(..))
import Data.Ord ((<))
import Data.Show (class Show, show)
import Data.String (length)
import DataModel.AppState (ProxyConnectionStatus(..))
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.Index (Index, emptyIndex)
import Effect.Class (liftEffect)
import Functions.Communication.StatelessOneTimeShare (PIN)
import Functions.EnvironmentalVariables (currentCommit)
import OperationalWidgets.UserAreaWidget (userAreaWidget)
import Views.CardsManagerView (CardManagerEvent, CardsManagerState, cardsManagerInitialState, cardsManagerView)
import Views.Components (footerComponent)
import Views.LoginFormView (credentialLoginWidget, pinLoginWidget)
import Views.OverlayView (OverlayInfo, overlay)
import Views.SignupFormView (SignupDataForm, emptyDataForm, signupFormView)

type SharedCardReference = String
type SharedCardPassword  = String
type Username = String

type LoginFormData = 
  { credentials :: Credentials
  , pin :: PIN
  , loginType :: LoginType
  }

data LoginType = CredentialLogin | PinLogin

emptyLoginFormData :: LoginFormData
emptyLoginFormData = { credentials: emptyCredentials, pin: "", loginType: CredentialLogin }


data LoginPageEvent   = LoginEvent Credentials
                      | LoginPinEvent PIN
                      | GoToCredentialLoginEvent Username
                      | GoToSignupEvent Credentials
data SignupPageEvent  = SignupEvent Credentials
                      | GoToLoginEvent Credentials

data PageEvent        = LoginPageEvent  LoginPageEvent
                      | SignupPageEvent SignupPageEvent
                      | CardManagerEvent CardManagerEvent
                      | UserAreaEvent UserAreaEvent

data UserAreaEvent    = UpdateUserPreferencesEvent -- ??
                      | ChangePasswordEvent -- ??
                      | SetPinEvent -- ??
                      | DeleteAccountEvent -- ??
                      | ImportCardsEvent -- List Card ??
                      | ExportJsonEvent -- ??
                      | ExportOfflineCopyEvent -- ??
                      | CloseUserArea CardsManagerState
                      | LockEvent
                      | LogoutEvent

data Page = Loading (Maybe Page) | Login LoginFormData | Signup SignupDataForm | Share (Maybe SharedCardReference) | Main MainPageWidgetState

type MainPageWidgetState = {
  index             :: Index
, showUserArea      :: Boolean
, cardsManagerState :: CardsManagerState
}

emptyMainPageWidgetState :: MainPageWidgetState
emptyMainPageWidgetState = { index: emptyIndex, showUserArea: false, cardsManagerState: cardsManagerInitialState }

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
                            Signup credentials' -> credentials'
                            _                   -> emptyDataForm
        
        (either GoToLoginEvent SignupEvent <$> (signupFormView credentials)) 
      ]
    , div [Props.classList (Just <$> ["page", "main", show $ location (Main emptyMainPageWidgetState) page])] [ do
        let {index, showUserArea, cardsManagerState} = case page of
                                        Main homePageWidgetState' -> homePageWidgetState'
                                        _                         -> emptyMainPageWidgetState
        
        div [Props._id "homePage"] [
          CardManagerEvent <$> cardsManagerView cardsManagerState index
        , (UserAreaEvent $ CloseUserArea cardsManagerState) <$ userAreaWidget (not showUserArea) ProxyOnline
        ]
         
      ]
    ]

    loginPage :: LoginFormData -> Widget HTML LoginPageEvent
    loginPage {credentials, pin, loginType} =
      case loginType of
        CredentialLogin -> either GoToSignupEvent LoginEvent <$> credentialLoginWidget credentials
        PinLogin        -> do
          form [Props.className "loginForm"] [
            div [Props.className "loginInputs"] [
              span [] [text "Enter your PIN"]
            , LoginPinEvent <$> pinLoginWidget (length pin < 5) pin
            , GoToCredentialLoginEvent credentials.username <$ a [Props.onClick] [text "Login with passphrase"]
            ] <|> ((GoToSignupEvent credentials) <$ button [Props.onClick] [text "sign up"])
          ]

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
  Main _ ,    Main _    -> CenterPosition

  Loading _,  _         -> LeftPosition
  Signup _,   Login _   -> LeftPosition
  Login _,    Share _   -> LeftPosition
  Signup _,   Share _   -> LeftPosition
  _,          Main  _   -> LeftPosition
  _,          _         -> RightPosition

pageClassName :: Page -> String
pageClassName (Loading _) = "loading"
pageClassName (Login _)   = "login"
pageClassName (Signup _)  = "signup"
pageClassName (Share _)   = "share"
pageClassName (Main _)  = "main"

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
  show (Main _)   = "Main"
