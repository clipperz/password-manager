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
import Data.Show (class Show, show)
import Data.Tuple (uncurry)
import DataModel.Index (Index, emptyIndex)
import DataModel.Password (PasswordGeneratorSettings, standardPasswordGeneratorSettings)
import Effect.Class (liftEffect)
import Functions.EnvironmentalVariables (currentCommit)
import Views.CardsManagerView (CardManagerEvent, CardManagerState, cardManagerInitialState, cardsManagerView)
import Views.Components (footerComponent)
import Views.LoginFormView (LoginFormData, LoginPageEvent, emptyLoginFormData, loginPage)
import Views.OverlayView (OverlayInfo, overlay)
import Views.SignupFormView (SignupDataForm, SignupPageEvent, emptyDataForm, signupFormView)
import Views.UserAreaView (UserAreaEvent, UserAreaState, userAreaInitialState, userAreaView)

data PageEvent        = LoginPageEvent           LoginPageEvent
                      | SignupPageEvent          SignupPageEvent
                      | MainPageCardManagerEvent CardManagerEvent CardManagerState
                      | MainPageUserAreaEvent    UserAreaEvent    CardManagerState UserAreaState

data Page = Loading (Maybe Page) | Login LoginFormData | Signup SignupDataForm | Main MainPageWidgetState

type MainPageWidgetState = {
  index                         :: Index
, userAreaState                 :: UserAreaState
, cardManagerState              :: CardManagerState
, userPasswordGeneratorSettings :: PasswordGeneratorSettings
}

loadingMainPage :: Index -> CardManagerState -> Page
loadingMainPage index cardManagerState = Main { index, userAreaState: userAreaInitialState, cardManagerState, userPasswordGeneratorSettings: standardPasswordGeneratorSettings }

emptyMainPageWidgetState :: MainPageWidgetState
emptyMainPageWidgetState = { index: emptyIndex, userAreaState: userAreaInitialState, cardManagerState: cardManagerInitialState, userPasswordGeneratorSettings: standardPasswordGeneratorSettings }

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
        
        (signupFormView credentials)
      ]
    , div [Props.classList (Just <$> ["page", "main", show $ location (Main emptyMainPageWidgetState) page])] [ do
        let {index, userAreaState, cardManagerState, userPasswordGeneratorSettings} = case page of
                                        Main homePageWidgetState' -> homePageWidgetState'
                                        _                         -> emptyMainPageWidgetState
        
        div [Props._id "homePage"] [
          ( MainPageCardManagerEvent                         # uncurry) <$> cardsManagerView cardManagerState index userPasswordGeneratorSettings
        , ((MainPageUserAreaEvent # flip $ cardManagerState) # uncurry) <$> userAreaView     userAreaState
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
  show (Loading _) = "Loading"
  show (Login _)   = "Login"
  show (Signup _)  = "Signup"
  show (Main _)    = "Main"
