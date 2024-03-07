module Test.Debug where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, span, text)
import Concur.React.Props as Props
import Control.Alternative ((*>))
import Control.Bind (bind)
import Data.Argonaut.Core (stringify)
import Data.Codec.Argonaut as CA
import Data.Eq ((==))
import Data.Function (($))
import Test.DebugCodec (widgetStateCodec)
import DataModel.WidgetState (WidgetState)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.EnvironmentalVariables (currentCommit)

foreign import formatJsonString :: String -> String

debugState :: forall a. WidgetState -> Widget HTML a
debugState widgetState = do
  commit <- liftEffect $ currentCommit
  let jsonEncodedState = stringify $ CA.encode widgetStateCodec widgetState
  _ <-  if   commit == "development" 
        then button [Props._id "DEBUG", Props.onClick] [span [] [text "DEBUG"]] *> (liftAff $ copyToClipboard jsonEncodedState)
        else emptyEl
  debugState widgetState

emptyEl :: forall a. Widget HTML a
emptyEl = text ""