module OperationalWidgets.App
  ( app
  , Page(..)
  , SharedCardReference
  )
  where

import Concur.Core (Widget)
import Concur.Core.FRP (Signal, demand, loopW, always, fireOnce, hold, dyn)
import Concur.React (HTML)
import Concur.React.DOM (div, text, ul, li, h1, h3, footer, header, span, a, button, div_, footer_, p_, ul_, li_, a_, h1_, h3_, header_, span_)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=), discard)
import Data.Array (range)
import Data.Formatter.Number (Formatter(..), format)
import Data.Function (($))
import Data.Functor (map, (<$), void, (<$>))
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Semigroup ((<>))
import Data.Show(class Show, show)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (sequence)
import Data.Unit (unit, Unit)
-- import Data.Unit (unit)
-- import DataModel.WidgetState (WidgetState(..))
import DataModel.Credentials (Credentials)
import Effect.Aff (delay, never)
import Effect.Aff.Class (liftAff)
-- import Effect.Class (liftEffect)
import Effect.Class.Console (log)
-- import Functions.State (computeInitialState)
-- import Functions.JSState (modifyAppState)
-- import OperationalWidgets.HomePageWidget as HomePageWidget
-- import Views.LoginFormView (LoginDataForm, emptyForm)
-- import Views.SignupFormView (SignupDataForm, emptyDataForm)
-- import Views.LandingPageView (landingPageView, LandingPageView(..))
import Views.LoginView ( loginViewSignal )

import Debug (traceM)

commitHash :: String
commitHash = "epsilon"

app :: forall a. Page -> Widget HTML a
app nextPage = app' (ShowPage (Loading (Just nextPage)))

-- ==================================================
{-
type PageStatus =
  { page    :: Page
  , login   :: LoginDataForm
  , signin  :: SignupDataForm
  }
-}

type SharedCardReference = String
type SharedCardPassword  = String

data Page = Loading (Maybe Page) | Login | Signup | Share (Maybe SharedCardReference) | Main
data Action = ShowPage Page | DoLogin Credentials | DoSignup Credentials | ShowSharedCard SharedCardReference SharedCardPassword | ShowMain

app' :: forall a. Action -> Widget HTML a
app' action = do
  traceM $ "Doing " <> show action
  nextAction:: Action <- exitBooting action <|>
    (demand $ div_ [Props.className "mainDiv"] do
      _ <- headerPage (actionPage action) (Loading Nothing) $ always unit
      loginPageRes <- headerPage (actionPage action) Login $ do
          loginRes <- ((<$>) DoLogin) <$> loginViewSignal
          changeViewRes <- fireOnce $ (ShowPage Signup) <$ button [Props.onClick] [text "=> signup"]
          traceM $ "loginRes " <> show loginRes
          pure $ loginRes <|> changeViewRes
      signupPageRes <- headerPage (actionPage action) Signup $ do
        -- DoSignup <$> signupView,
          fireOnce $ (ShowPage Login) <$ button [Props.onClick] [text "<= login"]
      sharePageRes <- div_ [Props.classList (Just <$> ["page", "main", show $ location (Share Nothing) (actionPage action)])] $ do
        _ <- div_ [Props.className "content"] (hold unit $ text "share")
        pure Nothing
      mainPageRes <- div_ [Props.classList (Just <$> ["page", "main", show $ location Main (actionPage action)])] $ do
        _ <- div_ [Props.className "content"] (hold unit $ text "main")
        pure Nothing
      pure $ loginPageRes <|> signupPageRes <|> sharePageRes <|> mainPageRes)
    <|>
    overlay { status: Hidden, message: "loading" }
  log $ "page action " <> show nextAction
  app' nextAction

-- ==================================================

data PagePosition = Left | Center | Right
instance showPagePosition :: Show PagePosition where
  show Left   = "left"
  show Center = "center"
  show Right  = "right"

location :: Page -> Page -> PagePosition
location referencePage currentPage = case currentPage, referencePage of
  Loading _,  Loading _ -> Center
  Login,      Login     -> Center
  Signup,     Signup    -> Center
  Share _,    Share _   -> Center
  Main,       Main      -> Center

  Loading _,  _         -> Left
  Login,      Signup    -> Left
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
actionPage (DoSignup _)         = Signup
actionPage (ShowSharedCard r _) = Share (Just r)
actionPage (ShowMain)           = Main

headerPage :: forall a. Page -> Page -> Signal HTML a -> Signal HTML a
headerPage currentPage page innerContent = do
  traceM $ "drawing headerPage"
  div_ [Props.classList (Just <$> ["page", pageClassName page, show $ location currentPage page])] do
    traceM "drawing content headerPage"
    div_ [Props.className "content"] do
      headerComponent
      res <- div_ [Props.className "content"] innerContent
      otherComponent
      footerComponent commitHash
      pure res

headerComponent :: Signal HTML Unit
headerComponent =
  header_ [] do
    h1_ [] (hold unit $ text "clipperz")
    h3_ [] (hold unit $ text "keep it to yourself")

otherComponent :: Signal HTML Unit
otherComponent =
  div_ [(Props.className "other")] do
    div_ [(Props.className "links")] do
      ul_ [] do
        _ <- li_ [] $ a_ [Props.href "https://clipperz.is/about/",          Props.target "_blank"] (hold unit $ text "About")
        _ <- li_ [] $ a_ [Props.href "https://clipperz.is/terms_service/",  Props.target "_blank"] (hold unit $ text "Terms of service")
        _ <- li_ [] $ a_ [Props.href "https://clipperz.is/privacy_policy/", Props.target "_blank"] (hold unit $ text "Privacy")
        pure unit

footerComponent :: String -> Signal HTML Unit
footerComponent commit =
  footer_ [] do
    div_ [Props.className "footerContent"] do
      div_ [Props.className "applicationVersion"] do
        span_ [] (hold unit $ text "application version")
        a_ [Props.href ("https://github.com/clipperz/password-manager/commit/" <> commit), Props.target "_black"] (hold unit $ text commit)
        pure unit

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


instance showPage :: Show Page where
  show (Loading _)  = "Loading"
  show (Login)      = "Login"
  show (Signup)     = "Signup"
  show (Share _)    = "Share"
  show (Main)       = "Main"

instance showAction :: Show Action where
  show (ShowPage page)      = "Show Page " <> show page
  show (DoLogin _)          = "Do Login"
  show (DoSignup _)         = "Do Signup"
  show (ShowSharedCard _ _) = "Show Shared Card"
  show (ShowMain)           = "Show Main"


{-
app :: Widget HTML Unit
app = do
  initialState <- liftEffect computeInitialState
  liftAff $ modifyAppState initialState
  _ <- do
    landingPageView (LoginView Default emptyForm)

    -- !!! AUTOLOGIN FOR DEVELOPING !!! --
    -- landingPageView (LoginView Loading {username: "joe", password: "clipperz"})
    -- -------------------------------- --

    void $ HomePageWidget.homePageWidget
  app
-}


{-

app :: Widget HTML Unit
app = app' Nothing

where
app' :: Maybe String -> Widget HTML Unit
app' maybeUsername = do
initialState' <- liftEffect $ runExceptT computeInitialState
case initialState' of
Right initialState@{proxy} -> do
liftAff $ modifyAppState initialState
let offlineCopyBanner = case proxy of
OfflineProxy _ -> [p [Props.className "notice"] [text "Offline copy"]]
_ -> []
res <- div [Props.className "wrapper"] $ offlineCopyBanner <> [
do
-- let form = fromMaybe emptyForm ((\u -> { username: u, password: "" }) <$> maybeUsername )
-- landingPageView (LoginView Default form)
-- !!! AUTOLOGIN FOR DEVELOPING !!! --
let form = fromMaybe {username: "joe", password: "clipperz"} ((\u -> { username: u, password: "" }) <$> maybeUsername )
landingPageView (LoginView Loading form)
-- -------------------------------- --
homePageWidget
]
case res of
Clean -> app' Nothing
ReadyForLogin username -> app' (Just username)

Left _ -> text "Could not initialize app"


-}
