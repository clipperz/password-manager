module AppMain
  ( main
  )
  where

import Concur.React.Run (runWidgetInDom)
import Control.Alternative (pure)
import Control.Bind (bind, discard, (>>=))
import Control.Monad.Except (runExceptT)
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
import Data.Unit (Unit, unit)
import DataModel.AppState (AppState)
import DataModel.FragmentData as Fragment
import Effect (Effect)
import Functions.JSState (modifyAppState)
import Functions.State (computeInitialState)
import JSURI (decodeURI)
import OperationalWidgets.App (Page(..), app, doTestLogin)
import Record (merge)
import Web.HTML (window)
import Web.HTML.Location (hash, setHash)
import Web.HTML.Window (location)

main :: Effect Unit
main = do
  l <- window >>= location
  h <- hash l
  maybeState <- computeStateWithFragmentData h
  case maybeState of
    Nothing -> pure unit
    Just state@{fragmentData} -> do
      modifyAppState state
      case fragmentData of
        Just Fragment.Registration -> runWidgetInDom "app" (app Signup)
        Just (Fragment.Login cred) -> runWidgetInDom "app" (doTestLogin cred)
        _                          -> do
                                      setHash "" l
                                      runWidgetInDom "app" (app (Loading (Just Login)))

parseQueryString :: String -> Array (Tuple String String)
parseQueryString query = 
  let keyValues = split (Pattern "&") query
      splitKeyValue = \s -> case split (Pattern "=") s of
                              [key, value] -> Just $ Tuple key value
                              _ -> Nothing
  in catMaybes $ splitKeyValue <$> keyValues 

computeStateWithFragmentData :: String -> Effect (Maybe AppState)
computeStateWithFragmentData fragment = do
  initialState <- runExceptT $ computeInitialState

  case initialState of
    Right state -> do
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
      pure $ Just (merge {fragmentData: fragmentData} state )      
    Left _ -> do
      pure Nothing