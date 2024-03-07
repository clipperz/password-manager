module Views.AppView where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, div, h1, h3, header, li, p, span, text, ul)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Bind (bind)
import Data.Function (flip, (#), ($))
import Data.Functor ((<$>))
import Data.Maybe (Maybe(..))
import Data.Monoid ((<>))
import Data.Newtype (unwrap)
import Data.Show (class Show, show)
import Data.Tuple (uncurry)
import DataModel.Credentials (emptyCredentials)
import DataModel.Index (emptyIndex)
import DataModel.User (defaultUserPreferences)
import DataModel.WidgetState (MainPageWidgetState, Page(..), UserAreaState, WidgetState(..), CardManagerState)
import Effect.Class (liftEffect)
import Functions.EnvironmentalVariables (currentCommit)
import Test.Debug (debugState)
import Views.CardsManagerView (CardManagerEvent, cardManagerInitialState, cardsManagerView)
import Views.Components (footerComponent, proxyInfoComponent)
import Views.LoginFormView (LoginPageEvent, emptyLoginFormData, loginPage)
import Views.OverlayView (overlay)
import Views.SignupFormView (SignupPageEvent, emptyDataForm, signupFormView)
import Views.UserAreaView (UserAreaEvent, userAreaInitialState, userAreaView)

emptyMainPageWidgetState :: MainPageWidgetState
emptyMainPageWidgetState = { index: emptyIndex, credentials: emptyCredentials, pinExists: false, userAreaState: userAreaInitialState, cardManagerState: cardManagerInitialState, userPreferences: defaultUserPreferences }

data PageEvent = LoginPageEvent           LoginPageEvent
               | SignupPageEvent          SignupPageEvent
               | MainPageCardManagerEvent CardManagerEvent CardManagerState
               | MainPageUserAreaEvent    UserAreaEvent    CardManagerState UserAreaState

appView :: WidgetState -> Widget HTML PageEvent
appView widgetState@(WidgetState overlayInfo page) =
  appPages <> debugState widgetState
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
        
        (signupFormView credentials)
      ]
    , div [Props.classList (Just <$> ["page", "main", show $ location (Main emptyMainPageWidgetState) page])] [ do
        let {index, userAreaState, credentials, pinExists, cardManagerState, userPreferences} = case page of
                                        Main homePageWidgetState' -> homePageWidgetState'
                                        _                         -> emptyMainPageWidgetState
        
        div [Props._id "homePage"] [
          ( MainPageCardManagerEvent                         # uncurry) <$> cardsManagerView cardManagerState index (unwrap userPreferences).passwordGeneratorSettings
        , ((MainPageUserAreaEvent # flip $ cardManagerState) # uncurry) <$> userAreaView userAreaState userPreferences credentials pinExists
        ] 
      ]
    ]

data PagePosition = LeftPosition | CenterPosition | RightPosition
instance showPagePosition :: Show PagePosition where
  show LeftPosition   = "left"
  show CenterPosition = "center"
  show RightPosition  = "right"

location :: Page -> Page -> PagePosition
location referencePage currentPage = case referencePage, currentPage of
  Loading _, Loading _ -> CenterPosition
  Login   _, Login   _ -> CenterPosition
  Signup  _, Signup  _ -> CenterPosition
  Main    _, Main    _ -> CenterPosition

  Loading _, _         -> LeftPosition
  Signup  _, Login   _ -> LeftPosition
  _,         Main    _ -> LeftPosition
  _,         _         -> RightPosition

pageClassName :: Page -> String
pageClassName (Loading _) = "loading"
pageClassName (Login _)   = "login"
pageClassName (Signup _)  = "signup"
pageClassName (Main _)    = "main"

headerPage :: forall a. Page -> Page -> Array (Widget HTML a) -> Widget HTML a
headerPage currentPage page innerContent = do
  commitHash <- liftEffect $ currentCommit
  div [Props.classList (Just <$> ["page", pageClassName page, show $ location page currentPage])] [
    div [Props.className "content"] [
      proxyInfoComponent []
    , headerComponent
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

