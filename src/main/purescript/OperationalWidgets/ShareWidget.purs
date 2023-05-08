module OperationalWidgets.ShareWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Maybe (Maybe)
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import Functions.Communication.OneTimeShare (share)
import Functions.EnvironmentalVariables (redeemURL)
import Views.ShareView (shareView)
import Web.HTML (window)
import Web.HTML.Location (setHash)
import Web.HTML.Window (location)

shareWidget :: Maybe String -> Widget HTML Unit
shareWidget secret = do
  pure $ unsafePerformEffect (setHash "" (unsafePerformEffect (location (unsafePerformEffect window))))
  (Tuple secret_ password_) <- shareView secret
  result <- liftAff $ runExceptT $ share secret_ password_
  redeemUrl <- liftEffect $ redeemURL
  case result of
    Left err -> text ("error:" <> show err)
    Right id -> text ("copy: " <> redeemUrl <> id)
    