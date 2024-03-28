module Test.Debug where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (button, span, text)
import Concur.React.Props as Props
import Control.Alt (void)
import Control.Alternative ((*>))
import Control.Bind (bind)
import Data.Argonaut.Core (stringify)
import Data.Codec.Argonaut as CA
import Data.Eq ((==))
import Data.Function ((#), ($))
import Data.Monoid ((<>))
import Data.Unit (Unit, unit)
import DataModel.WidgetState (WidgetState)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.EnvironmentalVariables (currentCommit)
import Test.DebugCodec (widgetStateCodec)

foreign import formatJsonString :: String -> String

foreign import _copyState :: Unit -> EffectFnAff Unit

copyState :: Aff Unit
copyState = fromEffectFnAff $ _copyState unit

debugState :: forall a. WidgetState -> Widget HTML a
debugState widgetState = do
  commit <- liftEffect $ currentCommit
  let jsonEncodedState = stringify $ CA.encode widgetStateCodec widgetState
  _ <-  if   commit == "development" 
        then ((button [Props._id "DEBUG", Props.onClick] [span [] [text "DEBUG"]] # void) <> (liftAff copyState)) *> (liftAff $ copyToClipboard jsonEncodedState)
        else emptyEl
  debugState widgetState

emptyEl :: forall a. Widget HTML a
emptyEl = text ""
