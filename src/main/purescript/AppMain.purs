module AppMain
  ( main
  )
  where

import Concur.React.Run (runWidgetInDom)
import Control.Bind (bind, discard, (>>=))
import Data.Argonaut.Decode (fromJsonString)
import Data.Array (catMaybes)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.FragmentState (FragmentState)
import DataModel.FragmentState as Fragment
import Effect (Effect)
import Foreign (unsafeToForeign)
import Functions.JSState (saveAppState)
import Functions.State (computeInitialState, computeInitialStatelessState)
import JSURI (decodeURI)
import OperationalWidgets.App (app)
import Web.HTML (Window, window)
import Web.HTML.History (DocumentTitle(..), URL(..), replaceState)
import Web.HTML.Location (hash, pathname)
import Web.HTML.Window (history, location)

-- ============================================
--  parse external state that is never updated
-- ============================================

main :: Effect Unit
main = do
  computeInitialState >>= saveAppState -- TODO REMOVE
  
  appState      <- computeInitialStatelessState
  fragmentState <- parseFragment <$> (window >>= location >>= hash)
  
  window >>= removeFragment
  
  runWidgetInDom "app" $ app appState fragmentState

-- ---------------------------------------------

removeFragment :: Window -> Effect Unit
removeFragment w = do
  pathName <- location w >>= pathname
  history w >>= replaceState (unsafeToForeign {}) (DocumentTitle "") (URL pathName)

parseFragment :: String -> FragmentState
parseFragment fragment = case fragment of
  "#registration" -> Fragment.Registration
  str -> do
    case split (Pattern "?") str of
      [ "#login", query ] -> do
        let parameters = fromFoldable $ parseQueryString query
        case (lookup "username" parameters), (lookup "password" parameters) of
          Just username, Just password -> Fragment.Login {username, password}
          _, _ -> Fragment.Unrecognized fragment
      [ "#addCard", cardData ] -> case fromJsonString (fromMaybe cardData (decodeURI cardData)) of
        Right card -> Fragment.AddCard card
        Left  _    -> Fragment.Unrecognized fragment
      _ -> Fragment.Empty

  where

    parseQueryString :: String -> Array (Tuple String String)
    parseQueryString query = 
      let keyValues = split (Pattern "&") query
          splitKeyValue = \s -> case split (Pattern "=") s of
                                  [key, value] -> Just $ Tuple key value
                                  _ -> Nothing
      in catMaybes $ splitKeyValue <$> keyValues 