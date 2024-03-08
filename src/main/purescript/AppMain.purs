module AppMain ( main ) where

import Concur.React.Run (runWidgetInDom)
import Control.Alternative (pure)
import Control.Bind (bind, discard, (=<<), (>>=))
import Control.Category ((<<<))
import Data.Argonaut.Parser (jsonParser)
import Data.Array (catMaybes)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut (JsonDecodeError(..), decode)
import Data.Either (Either(..), note)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, hex)
import Data.Map (fromFoldable, lookup)
import Data.Maybe (Maybe(..))
import Data.String (split)
import Data.String.Pattern (Pattern(..))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.CardVersions.Card (toCard)
import DataModel.CardVersions.CurrentCardVersions (currentCardCodecVersion)
import DataModel.FragmentState (FragmentState)
import DataModel.FragmentState as Fragment
import Effect (Effect)
import Foreign (unsafeToForeign)
import Functions.Pin (makeKey)
import Functions.State (computeInitialState)
import JSURI (decodeURI)
import OperationalWidgets.App (app)
import Record (merge)
import Web.HTML (Window, window)
import Web.HTML.History (DocumentTitle(..), URL(..), replaceState)
import Web.HTML.Location (hash, pathname)
import Web.HTML.Window (history, localStorage, location)
import Web.Storage.Storage (getItem)

main :: Effect Unit
main = do
  appState      <- computeInitialState
  fragmentState <- parseFragment <$> (window >>= location >>= hash)
  
  credentials   <- getCredentialsFromLocalStorage

  window >>= removeFragment
  
  runWidgetInDom "app" $ app (merge credentials appState) fragmentState

-- ---------------------------------------------

getCredentialsFromLocalStorage :: Effect { username :: Maybe String, pinEncryptedPassword :: Maybe HexString }
getCredentialsFromLocalStorage = do
  storage    <- window >>= localStorage
  user       <- getItem (makeKey "user")       storage
  passphrase <- getItem (makeKey "passphrase") storage
  pure $ {username: user, pinEncryptedPassword: hex <$> passphrase}

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
      [ "#addCard", cardData ] -> case toCard <$> (decode currentCardCodecVersion =<< (lmap TypeMismatch <<< jsonParser) =<< (note MissingValue $ decodeURI cardData)) of
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