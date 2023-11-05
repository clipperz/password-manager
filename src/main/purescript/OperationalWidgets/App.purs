module OperationalWidgets.App
  ( Page(..)
  , SharedCardReference
  , app
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, h1, h3, header, li, p, span, text, ul)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Alternative ((<*))
import Control.Applicative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except.Trans (runExceptT)
import Data.Either (either)
import Data.Either as E
import Data.Eq ((==), class Eq)
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Time.Duration (Milliseconds(..))
import DataModel.AppState (AppError, UserConnectionStatus(..))
import DataModel.Card (Card)
import DataModel.Credentials (Credentials, emptyCredentials)
import DataModel.FragmentState as Fragment
import DataModel.StatelessAppState (AppStateResponse(..), ProxyResponse(..), StatelessAppState)
import DataModel.WidgetState as WS
import Effect.Aff (delay, never, Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Signup (signupUser)
import Functions.EnvironmentalVariables (currentCommit)
import Functions.Login (doLogin)
import Functions.Pin (decryptPassphraseWithRemoval)
import OperationalWidgets.HomePageWidget (homePageWidget)
import Record (merge)
import Views.Components (footerComponent)
import Views.LoginFormView (loginFormView, PinCredentials)
import Views.OverlayView (OverlayStatus(..), overlay)
import Views.SignupFormView (emptyDataForm, signupFormView)
import Web.HTML (window)
import Web.HTML.Window (localStorage)

-- ========================================================
--  manage application effects (state, local storage, api)
-- ========================================================

app :: forall a. StatelessAppState -> Fragment.FragmentState -> Widget HTML a
app appState fragmentState = case fragmentState of
    Fragment.Registration -> app' appState (ShowPage (Loading (Just Signup))) emptyCredentials
    Fragment.Login cred   -> app' appState (DoLogin cred)                     cred
    _                     -> app' appState (ShowPage (Loading (Just Login)))  emptyCredentials

  where

    app' :: forall a'. StatelessAppState -> Action -> Credentials -> Widget HTML a'
    app' state action credentials = do
      log $ show action
      AppStateResponse newState nextAction <- 
        AppStateResponse state <$> appView action credentials (extractCardFromFragment fragmentState)
        <|>
        (liftAff $ doOp state action)
      app' newState nextAction $ case nextAction of
        DoLogin cred  -> cred
        ShowPage Main -> emptyCredentials
        _             -> credentials

    extractCardFromFragment :: Fragment.FragmentState -> Maybe Card
    extractCardFromFragment (Fragment.AddCard card) = Just card
    extractCardFromFragment _                       = Nothing

doOp :: StatelessAppState -> Action -> Aff (AppStateResponse Action)
doOp state@{proxy, hash, srpConf} (DoSignup cred) = do
  let res = signupUser proxy hash srpConf cred
  (either (\err -> AppStateResponse state (ShowError Signup err)) (\(ProxyResponse newProxy _) -> AppStateResponse (state {proxy = newProxy}) (DoLogin cred))) <$> (runExceptT res)
doOp state@{proxy, hash, srpConf} (DoLogin cred) = do
  res <- runExceptT $ doLogin proxy hash srpConf cred
  pure $ (either (\err -> AppStateResponse state (ShowError Login err)) (\stateUpdate -> AppStateResponse (merge stateUpdate state) (ShowSuccess Main)) res)
doOp state (DoLoginWithPin {pin, user, passphrase}) = do
  _   <- liftEffect $ window >>= localStorage
  res <- runExceptT $ decryptPassphraseWithRemoval pin user passphrase
  pure $ AppStateResponse state (either (ShowError Login) DoLogin res)
doOp state (ShowSuccess nextPage)   = (AppStateResponse state $ ShowPage nextPage) <$ delay (Milliseconds 500.0)
doOp state (ShowError   nextPage _) = (AppStateResponse state $ ShowPage nextPage) <$ delay (Milliseconds 500.0)
doOp state a = AppStateResponse state a <$ never


-- ==================================================
--                  ui elements
-- ==================================================

type SharedCardReference = String
type SharedCardPassword  = String

data Page = Loading (Maybe Page) | Login | Signup | Share (Maybe SharedCardReference) | Main

data Action = DoLogout
            | ShowPage Page
            | DoLogin Credentials
            | DoLoginWithPin PinCredentials
            | DoSignup Credentials
            | ShowSharedCard SharedCardReference SharedCardPassword
            | ShowError Page AppError
            | ShowSuccess Page

appView :: Action -> Credentials -> Maybe Card -> Widget HTML Action
appView action credentials cardToAdd =
  (div [Props.className "mainDiv"] [
      headerPage (actionPage action) (Loading Nothing) []
    , headerPage (actionPage action) Login [
        (getLoginActionType <$> (loginFormView credentials)) <|> ((ShowPage Signup) <$ button [Props.onClick] [text "sign up"]) -- TODO: if login fails this is reset
      ]
    , headerPage (actionPage action) Signup [
        (DoSignup <$> (signupFormView WS.Default $ merge credentials emptyDataForm)) <|> ((ShowPage Login) <$ button [Props.onClick] [text "login"])
      ]
    , div [Props.classList (Just <$> ["page", "share", show $ location (Share Nothing) (actionPage action)])] [
        div [Props.className "content"] [text "share"]
      ]
    , div [Props.classList (Just <$> ["page", "main", show $ location Main (actionPage action)])] [
      DoLogout <$ (homePageWidget (if (action == (ShowPage Main)) then UserLoggedIn else UserAnonymous) cardToAdd)
    ]
  ])
  <|>
  exitBooting action
  <|>
  overlayFromAction action

  where 
    getLoginActionType :: E.Either PinCredentials Credentials -> Action
    getLoginActionType (E.Left pinCredentials) = DoLoginWithPin pinCredentials
    getLoginActionType (E.Right credentials_)  = DoLogin credentials_

    exitBooting :: Action -> Widget HTML Action
    exitBooting (ShowPage (Loading (Just nextPage))) = liftAff $ (ShowPage nextPage) <$ delay (Milliseconds 1.0)
    exitBooting action'                              = liftAff $ action'             <$ never

    overlayFromAction :: forall a. Action -> Widget HTML a
    overlayFromAction (DoLogin _)        = overlay { status: Spinner, message: "loading" }
    overlayFromAction (DoLoginWithPin _) = overlay { status: Spinner, message: "loading" }
    overlayFromAction (DoSignup _)       = overlay { status: Spinner, message: "loading" }
    overlayFromAction (ShowError _ err)  = overlay { status: Failed,  message: "error"   } <* (log $ show err)
    overlayFromAction (ShowSuccess _)    = overlay { status: Done,    message: ""        }
    overlayFromAction _                  = overlay { status: Hidden,  message: "loading" }

data PagePosition = Left | Center | Right
instance showPagePosition :: Show PagePosition where
  show Left   = "left"
  show Center = "center"
  show Right  = "right"

location :: Page -> Page -> PagePosition
location referencePage currentPage = case referencePage, currentPage of
  Loading _,  Loading _ -> Center
  Login,      Login     -> Center
  Signup,     Signup    -> Center
  Share _,    Share _   -> Center
  Main,       Main      -> Center

  Loading _,  _         -> Left
  Signup,     Login     -> Left
  Login,      Share _   -> Left
  Signup,     Share _   -> Left
  _,          Main      -> Left
  _,          _         -> Right

pageClassName :: Page -> String
pageClassName (Loading _) = "loading"
pageClassName Login       = "login"
pageClassName Signup      = "signup"
pageClassName (Share _)   = "share"
pageClassName Main        = "main"

-- ==================================================

actionPage :: Action -> Page
actionPage (ShowPage page)      = page
actionPage (DoLogin _)          = Login
actionPage (DoLoginWithPin _)   = Login
actionPage (DoSignup _)         = Signup
actionPage (ShowSharedCard r _) = Share (Just r)
actionPage (DoLogout)           = Login
actionPage (ShowError page _)     = page
actionPage (ShowSuccess page)   = page

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
  show (Login)      = "Login"
  show (Signup)     = "Signup"
  show (Share _)    = "Share"
  show (Main)       = "Main"

derive instance eqPage :: Eq Page

instance showAction :: Show Action where
  show (ShowPage page)      = "Show Page " <> show page
  show (DoLogin _)          = "Do Login"
  show (DoLoginWithPin _)   = "Do LoginWithPint"
  show (DoSignup _)         = "Do Signup"
  show (ShowSharedCard _ _) = "Show Shared Card"
  show (ShowError page _)   = "Show Error " <> show page 
  show (ShowSuccess page)   = "Show Success " <> show page 
  show DoLogout = "DoLogout"

derive instance eqAction :: Eq Action
