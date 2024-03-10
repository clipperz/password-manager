module Test.DebugAppMain (main) where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, fireOnce, loopW)
import Concur.React (HTML)
import Concur.React.DOM (button, div, span, text, textarea)
import Concur.React.Props as Props
import Concur.React.Run (runWidgetInDom)
import Control.Alt (($>), (<#>), (<$), (<$>))
import Control.Alternative (pure, (*>))
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Category ((<<<))
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonDecodeError(..), printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Function (($))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (Pattern(..), stripPrefix)
import Data.Unit (Unit, unit)
import DataModel.WidgetState (WidgetState)
import Effect (Effect)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Foreign (unsafeToForeign)
import Functions.Clipboard (getClipboardContent)
import JSURI (decodeURIComponent)
import Test.Debug (formatJsonString)
import Test.DebugCodec (widgetStateCodec)
import Views.AppView (appView)
import Web.HTML (window)
import Web.HTML.History (DocumentTitle(..), URL(..), replaceState)
import Web.HTML.Location (hash, pathname, setHash)
import Web.HTML.Window (history, location)

main :: Effect Unit
main = do  
  runWidgetInDom "debugApp" $ debugApp Nothing

debugApp :: Maybe WidgetState -> Widget HTML Unit

debugApp Nothing = do
  hash_ <- liftEffect $ (hash =<< location =<< window) <#> (\s -> stripPrefix (Pattern "#") s >>= decodeURIComponent)
  (maybe (liftAff $ getClipboardContent) (pure <<< Just) hash_) >>= getWidgetState true Nothing >>= loop

debugApp defaultState = getWidgetState false Nothing ((stringify <<< CA.encode widgetStateCodec) <$> defaultState) >>= loop

loop :: WidgetState -> Widget HTML Unit
loop widgetState = do
  _ <- demand $ do
    loopW unit (\_ -> unit <$ appView widgetState)
    fireOnce (button [Props.onClick, Props._id "DEBUG_MODIFY"] [text "Modify State"])
  debugApp (Just widgetState)

getWidgetState :: Boolean -> Maybe JsonDecodeError -> Maybe String -> Widget HTML WidgetState
getWidgetState autoSubmit maybeError maybeDefault = do
  jsonState <- case maybeDefault, autoSubmit of
    Just default, true -> pure default
    _           , _    -> div [Props._id "DEBUG_INPUT"] [
      div [] [span [] [text $ fromMaybe "" (printJsonDecodeError <$> maybeError)]]
    , demand $ do
        v <- loopW (fromMaybe "" maybeDefault) (\content -> 
          textarea [
            Props.value $ formatJsonString content
          , Props.onChange
          ] [] <#> Props.unsafeTargetValue
        )

        fireOnce (button [Props._id "DEBUG_LOAD", Props.onClick $> v] [text "Load App"])
    ]
  decodeWidgetStateJson jsonState

decodeWidgetStateJson :: String -> Widget HTML WidgetState
decodeWidgetStateJson jsonEncodedState = do
  let decodeResult = CA.decode widgetStateCodec =<< (lmap TypeMismatch $ jsonParser jsonEncodedState)
  case decodeResult of
    Left  err         -> liftEffect removeFragment *> getWidgetState false (Just err) (Just jsonEncodedState)
    Right widgetState -> do
      liftEffect $ setHash jsonEncodedState =<< location =<< window
      pure widgetState
  
  where
    removeFragment :: Effect Unit
    removeFragment = do
      pathName <- window >>= location >>= pathname
      window >>= history >>= replaceState (unsafeToForeign {}) (DocumentTitle "") (URL pathName)
