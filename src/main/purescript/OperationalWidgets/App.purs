module OperationalWidgets.App
  ( Page(..)
  , SharedCardReference
  , app
  , doTestLogin
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, h1, h3, header, li, p, span, text, ul)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)
import Control.Monad.Except.Trans (runExceptT)
import Data.Array (range)
import Data.Either as E
import Data.Eq ((==), class Eq)
import Data.Formatter.Number (Formatter(..), format)
import Data.Function (($))
import Data.Functor (map, (<$), (<$>))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (class Show, show)
import Data.Time.Duration (Milliseconds(..))
import DataModel.AppState (UserConnectionStatus(..))
import DataModel.Credentials (Credentials)
import DataModel.WidgetState as WS
import Effect.Aff (delay, never, Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Signup (signupUser)
import Functions.EnvironmentalVariables (currentCommit)
import Functions.JSState (modifyAppState)
import Functions.Login (doLogin)
import Functions.Pin (decryptPassphraseWithRemoval)
import Functions.State (computeInitialState)
import OperationalWidgets.HomePageWidget (homePageWidget)
import Record (merge)
import Views.Components (footerComponent)
import Views.LoginFormView (loginFormView', PinCredentials)
import Views.SignupFormView (emptyDataForm, signupFormView)
import Web.HTML (window)
import Web.HTML.Window (localStorage)

emptyCredentials :: Credentials
emptyCredentials = { username: "", password: "" }

app :: forall a. Page -> Widget HTML a
app nextPage = app' (InitState (ShowPage (Loading (Just nextPage)))) { credentials: emptyCredentials }

doTestLogin :: forall a. String -> String -> Widget HTML a
doTestLogin username password = do
  log "Do test login"
  app' (InitState (DoLogin testCredentials)) { credentials: testCredentials }
  where
    testCredentials = { username: username, password: password}

type SharedCardReference = String
type SharedCardPassword  = String

data Page = Loading (Maybe Page) | Login | Signup | Share (Maybe SharedCardReference) | Main
data Action = DoLogout 
            | ShowPage Page 
            | DoLogin Credentials 
            | DoLoginWithPin PinCredentials 
            | DoSignup Credentials 
            | ShowSharedCard SharedCardReference SharedCardPassword 
            | ShowError Page 
            | ShowSuccess Page 
            | InitState Action

app' :: forall a. Action -> { credentials :: Credentials } -> Widget HTML a
app' action st@{ credentials } = do
  log $ show action
  nextAction:: Action <- exitBooting action <|>
    div [Props.className "mainDiv"] [
        headerPage (actionPage action) (Loading Nothing) []
      , headerPage (actionPage action) Login [
          (getLoginActionType <$> (loginFormView' credentials)) <|> ((ShowPage Signup) <$ button [Props.onClick] [text "sign up"]) -- TODO: if login fails this is reset
          {-
            Even when using Signals, the moment the signal terminates because the user clicks "login" its value is lost, because the signal will be drawn anew
          -}
        ]
      , headerPage (actionPage action) Signup [
          (DoSignup <$> (signupFormView WS.Default $ merge credentials emptyDataForm)) <|> ((ShowPage Login) <$ button [Props.onClick] [text "login"])
        ]
      , div [Props.classList (Just <$> ["page", "share", show $ location (Share Nothing) (actionPage action)])] [
          div [Props.className "content"] [text "share"]
        ]
      , div [Props.classList (Just <$> ["page", "main", show $ location Main (actionPage action)])] [
        DoLogout <$ (homePageWidget $ (if (action == (ShowPage Main)) then UserLoggedIn else UserAnonymous))
      ]
    ]
    <|>
    overlayFromAction action
    <|> 
    (liftAff $ doOp action)
  -- log $ "page action " <> show nextAction
  case nextAction of
    DoLogin cred -> app' nextAction $ st { credentials = cred }
    ShowPage Main -> app' nextAction $ st { credentials = emptyCredentials }
    _ -> app' nextAction st

  where 
    getLoginActionType :: E.Either PinCredentials Credentials -> Action
    getLoginActionType (E.Left pinCredentials) = DoLoginWithPin pinCredentials
    getLoginActionType (E.Right credentials_) = DoLogin credentials_

overlayFromAction :: forall a. Action -> Widget HTML a
overlayFromAction (DoLogin _)        = overlay { status: Spinner, message: "loading" }
overlayFromAction (DoLoginWithPin _) = overlay { status: Spinner, message: "loading" }
overlayFromAction (DoSignup _)       = overlay { status: Spinner, message: "loading" }
overlayFromAction (ShowError _)      = overlay { status: Failed, message: "error" }
overlayFromAction (ShowSuccess _)    = overlay { status: Done, message: "" }
overlayFromAction (InitState _)      = overlay { status: Spinner, message: "loading "}
overlayFromAction _ = overlay { status: Hidden, message: "loading" }
-- ==================================================

data PagePosition = Left | Center | Right
instance showPagePosition :: Show PagePosition where
  show Left   = "left"
  show Center = "center"
  show Right  = "right"

-- pageOrder :: List Page
-- pageOrder = (Loading Nothing) : Login : Signup : (Share Nothing) : Main : Nil

location :: Page -> Page -> PagePosition
location referencePage currentPage = case referencePage, currentPage of
  Loading _,  Loading _ -> Center
  Login,      Login     -> Center
  Signup,     Signup    -> Center
  Share _,    Share _   -> Center
  Main,       Main      -> Center

  Loading _,  _         -> Left
  Login,      Signup    -> Left
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

exitBooting :: Action -> Widget HTML Action
exitBooting (ShowPage (Loading (Just nextPage)))  = liftAff $ (ShowPage nextPage) <$ delay (Milliseconds 1.0)
exitBooting action                                = liftAff $ action              <$ never

-- ==================================================

actionPage :: Action -> Page
actionPage (ShowPage page)      = page
actionPage (DoLogin _)          = Login
actionPage (DoLoginWithPin _)   = Login
actionPage (DoSignup _)         = Signup
actionPage (ShowSharedCard r _) = Share (Just r)
-- actionPage (ShowMain)           = Main
actionPage (DoLogout)           = Login
actionPage (ShowError page)     = page
actionPage (ShowSuccess page)   = page
actionPage (InitState action)   = Loading (Just (actionPage action))

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

data OverlayStatus = Hidden | Spinner | Done | Failed -- | ProgressBar | Custom
type OverlayInfo = { status :: OverlayStatus, message :: String }

overlay :: forall a. OverlayInfo -> Widget HTML a
overlay info =
  div [Props.classList (Just <$> ["overlay", visibility])] [
    case info.status of
      Hidden  ->  div  [] []
      Spinner ->  div  [Props.className "spinner"]     $ map (\i -> div [Props.className ("bar" <> (format dd (toNumber i)))] []) (range 1 12)
      Done    ->  span [Props.className "icon done"]   [text "done"]
      Failed  ->  span [Props.className "icon failed"] [text "failed"]
    ,
    span [Props.className "title"] [text info.message]
  ]
  where
    dd :: Formatter
    dd  = Formatter { comma: false, before: 2, after: 0, abbreviations: false, sign: false }

    visibility :: String
    visibility = case info.status of
      Hidden  -> "hidden"
      _       -> "visible"

doOp :: Action -> Aff Action
doOp (DoLogin cred) = do
  res <- runExceptT $ doLogin cred
  case res of
    E.Right _ -> pure $ ShowSuccess Main
    E.Left err -> do
      log $ "Login error: " <> (show err)
      pure $ ShowError Login
doOp (DoLoginWithPin {pin, user, passphrase}) = do
  _  <- liftEffect $ window >>= localStorage
  ei <- runExceptT $ decryptPassphraseWithRemoval pin user passphrase
  case ei of
    E.Right cred -> pure (DoLogin cred)
    E.Left e -> do
      log $ show e
      pure $ ShowError Login
doOp (ShowSuccess nextPage) = (ShowPage nextPage) <$ delay (Milliseconds 500.0)
doOp (ShowError nextPage)   = (ShowPage nextPage) <$ delay (Milliseconds 500.0)
doOp (DoSignup cred) = do
  res <- liftAff $ runExceptT $ signupUser cred
  case res of
    E.Right _ -> pure $ DoLogin cred
    E.Left err -> do
      log $ "Signup error: " <> (show err)
      pure $ ShowPage Signup
doOp (InitState action) = do
  initialState <- liftEffect $ runExceptT $ computeInitialState
  case initialState of
    E.Right st -> do
      modifyAppState st
      pure action
    E.Left err -> do
      log $ show err
      pure action
doOp a = a <$ never

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
  show (ShowError page)     = "Show Error " <> show page 
  show (ShowSuccess page)   = "Show Success " <> show page 
  show DoLogout = "DoLogout"
  show (InitState action)   = "Show InitState " <> show action

derive instance eqAction :: Eq Action
