module Main
  ( main
  )
  where

import Concur.React.Run (runWidgetInDom)
import Control.Bind (bind, discard, (>>=))
import Data.Array (index, catMaybes)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.String (drop, split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect (Effect)
import OperationalWidgets.App (app, Page(..), SharedCardReference, doTestLogin)
import Web.HTML (window)
import Web.HTML.Location (hash, search)
import Web.HTML.Window (location)

import Effect.Class.Console (log)
import Data.Semigroup ((<>))
import Data.Show (show)

main :: Effect Unit
main = do
  l <- window >>= location
  h <- hash l
  s <- search l
  case h of
    "#registration" -> runWidgetInDom "app" (app Signup)
    str -> do
      case split (Pattern "?") str of
        [ "#login", query ] -> do
          let parameters = fromFoldable $ parseQueryString query
          log $ show parameters
          case (lookup "username" parameters), (lookup "password" parameters) of
            Just u, Just p -> runWidgetInDom "app" (doTestLogin u p)
            _, _ -> defaultPage
        [ "#share", query ] -> do
          let parameters = fromFoldable $ parseQueryString query
          case lookup "token" parameters of
            Just token -> runWidgetInDom "app" (app (Share (Just token)))
            _ -> defaultPage
        r -> defaultPage

defaultPage :: Effect Unit
defaultPage = runWidgetInDom "app" (app (Loading (Just Login)))

parseQueryString :: String -> Array (Tuple String String)
parseQueryString query = 
  let keyValues = split (Pattern "&") query
      splitKeyValue = \s -> case split (Pattern "=") s of
                              [key, value] -> Just $ Tuple key value
                              _ -> Nothing
  in catMaybes $ splitKeyValue <$> keyValues 
