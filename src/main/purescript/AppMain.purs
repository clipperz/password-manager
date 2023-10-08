module AppMain
  ( main
  )
  where

import Concur.React.Run (runWidgetInDom)
import Control.Alternative (pure)
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
import DataModel.AppState (AppState)
import DataModel.FragmentData as Fragment
import Effect (Effect)
import Foreign (unsafeToForeign)
import Functions.JSState (modifyAppState)
import Functions.State (computeInitialState)
import JSURI (decodeURI)
import OperationalWidgets.App (Page(..), app, doTestLogin)
import Record (merge)
import Web.HTML (Window, window)
import Web.HTML.History (DocumentTitle(..), URL(..), replaceState)
import Web.HTML.Location (hash, pathname)
import Web.HTML.Window (history, location)

main :: Effect Unit
main = do
  state@{fragmentData} <- window >>= location >>= hash >>= computeStateWithFragmentData
  modifyAppState state
  window >>= removeHash
  runWidgetInDom "app" case fragmentData of
    Just (Fragment.Login cred) -> doTestLogin cred --test shortcut
    Just Fragment.Registration -> app Signup
    _                          -> app (Loading (Just Login))

  where
    removeHash :: Window -> Effect Unit
    removeHash w = do
      pathName <- location w >>= pathname
      history w >>= replaceState (unsafeToForeign {}) (DocumentTitle "") (URL pathName)

parseQueryString :: String -> Array (Tuple String String)
parseQueryString query = 
  let keyValues = split (Pattern "&") query
      splitKeyValue = \s -> case split (Pattern "=") s of
                              [key, value] -> Just $ Tuple key value
                              _ -> Nothing
  in catMaybes $ splitKeyValue <$> keyValues 

computeStateWithFragmentData :: String -> Effect AppState
computeStateWithFragmentData fragment = do
  initialState <- computeInitialState
  fragmentData <- pure $ case fragment of
    "#registration" -> Just Fragment.Registration
    str -> do
      case split (Pattern "?") str of
        [ "#login", query ] -> do
          let parameters = fromFoldable $ parseQueryString query
          case (lookup "username" parameters), (lookup "password" parameters) of
            Just username, Just password -> Just $ Fragment.Login {username, password}
            _, _ -> Just $ Fragment.Unrecognized fragment
        [ "#addCard", cardData ] -> case fromJsonString (fromMaybe cardData (decodeURI cardData)) of
          Right card -> Just $ Fragment.AddCard card
          Left  _    -> Just $ Fragment.Unrecognized fragment
        _ -> Nothing
  pure (merge {fragmentData: fragmentData} initialState )      
