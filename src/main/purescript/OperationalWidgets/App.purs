module OperationalWidgets.App where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text, ul, li, h1, h3, footer, header, span, a, button)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Bind (bind, discard)
import Data.Array (range)
import Data.Formatter.Number (Formatter(..), format)
import Data.Function (($))
import Data.Functor (map, (<$), (<$>))
import Data.Maybe (Maybe(..))
import Data.Int (toNumber)
import Data.Semigroup ((<>))
import Data.Show(class Show, show)
import Data.Time.Duration (Milliseconds(..))
-- import Data.Unit (unit)
-- import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (delay, never)
import Effect.Aff.Class (liftAff)
-- import Effect.Class (liftEffect)
import Effect.Class.Console (log)
-- import Functions.State (computeInitialState)
-- import Functions.JSState (modifyAppState)
-- import OperationalWidgets.HomePageWidget as HomePageWidget
-- import Views.LoginFormView (emptyForm)
-- import Views.LandingPageView (landingPageView, LandingPageView(..))

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

commitHash :: String
commitHash = "epsilon"

data Page = Loading | Login | Signup | Main

data PagePosition = Left | Center | Right
instance showPagePosition :: Show PagePosition where
  show Left   = "left"
  show Center = "center"
  show Right  = "right"

pageClassName :: Page -> String
pageClassName Loading = "loading"
pageClassName Login   = "login"
pageClassName Signup  = "signup"
pageClassName Main    = "main"

headerPage :: forall a. Page -> Page -> Array (Widget HTML a) -> Widget HTML a
headerPage currentPage page innerContent =
  div [Props.classList (Just <$> ["page", pageClassName page, location currentPage page])] [
    div [Props.className "content"] [
      headerComponent,
      div [Props.className "content"] innerContent,
      otherComponent,
      footerComponent commitHash
    ]
  ]
    

app :: forall a. Page -> Widget HTML a
app page = do
  newPage:: Page <- exitBooting page <|>
    div [Props.className "mainDiv"] [
      headerPage page Loading [],
      headerPage page Login   [
        text "login", Signup <$ button [Props.onClick] [text "=> signup"]
      ],
      headerPage page Signup  [
        text "signup", Login <$ button [Props.onClick] [text "<= login"]
      ],
      div [Props.classList (Just <$> ["page", "main", location Main page])] [
        div [Props.className "content"] [ text "main" ]
      ]
    ]
    <|>
    overlay { status: Hidden, message: "loading" }
  log $ "changing page " <> pageClassName newPage
  app newPage

-- ==================================================

exitBooting :: Page -> Widget HTML Page
exitBooting Loading = liftAff $ Login   <$ delay (Milliseconds 1.0)
exitBooting _       = liftAff $ Loading <$ never

headerComponent :: forall a. Widget HTML a
headerComponent =
  header [] [
    h1 [] [text "clipperz"],
    h3 [] [text "keep it to yourself"]
  ]

otherComponent :: forall a. Widget HTML a
otherComponent =
  div [(Props.className "other")] [
    div [(Props.className "links")] [
      ul [] [
        li [] [a [Props.href "https://clipperz.is/about/",          Props.target "_blank"] [text "About"]],
        li [] [a [Props.href "https://clipperz.is/terms_service/",  Props.target "_blank"] [text "Terms of service"]],
        li [] [a [Props.href "https://clipperz.is/privacy_policy/", Props.target "_blank"] [text "Privacy"]]
      ]
    ]
  ]

footerComponent :: forall a. String -> Widget HTML a
footerComponent commit =
  footer [] [
    div [Props.className "footerContent"] [
      div [Props.className "applicationVersion"] [
        span [] [text "application version"],
        a [Props.href ("https://github.com/clipperz/password-manager/commit/" <> commit), Props.target "_black"] [text commit]
      ]
    ]
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



location :: Page -> Page -> String
location referencePage currentPage = show $ case currentPage, referencePage of
  Loading,  Loading -> Center
  Login,    Login   -> Center
  Signup,   Signup  -> Center
  Main,     Main    -> Center

  Loading,  _       -> Left
  Login,    Signup  -> Left
  _,        Main    -> Left
  _,        _       -> Right
