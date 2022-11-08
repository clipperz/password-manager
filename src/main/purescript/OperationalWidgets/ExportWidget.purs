module OperationalWidgets.ExportWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, a, text, button)
import Concur.React.Props as Props
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except (runExcept)
import Control.Semigroupoid ((<<<))
import Data.Argonaut.Core as AC
import Data.Either (either, Either(..))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.Maybe (fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Monoid (mempty)
import Data.Show (show)
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Aff (Aff, makeAff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Foreign (readString)
import Functions.Events (renderElement)
import Web.File.Blob (fromString, Blob)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.File.File (File, name)
import Web.File.FileReader (fileReader, readAsText, toEventTarget, result)
import Web.File.Url (createObjectURL, revokeObjectURL)
import Web.HTML (window)
import Web.HTML.Event.EventTypes as EventTypes
import Web.HTML.HTMLDocument (documentElement)
import Web.HTML.HTMLHtmlElement (toHTMLElement)
import Web.HTML.HTMLElement (toElement)
import Web.HTML.Window (document)

exportWidget :: Widget HTML Unit
exportWidget = do
  blob <- liftEffect $ prepareHTMLBlob
  s :: String <- liftAff $ readFile blob
  url <- liftEffect $ createObjectURL blob
  div [] [a [Props.download "clipperz_data", Props.href url, (\_ -> unit) <$> Props.onClick] [text "export"]]

prepareHTMLBlob :: Effect Blob
prepareHTMLBlob = do
  w <- window
  doc <- document w
  html <- ((<$>) (renderElement <<< toElement <<< toHTMLElement)) <$> documentElement doc
  pure $ fromString (fromMaybe "<html></html>" html) (MediaType "text/html")

readFile :: Blob -> Aff String
readFile blob = makeAff \fun -> do
  let err = fun <<< Left
      succ = fun <<< Right
  fr <- fileReader
  let et = toEventTarget fr

  errorListener <- eventListener \_ -> err (error "error")
  loadListener <- eventListener \_ -> do
    res <- result fr
    either (\errs -> err $ error $ show errs) succ $ runExcept $ readString res

  addEventListener EventTypes.error errorListener false et
  addEventListener EventTypes.load loadListener false et
  readAsText blob fr
  pure mempty
