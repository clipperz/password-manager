module OperationalWidgets.ShareWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, p, text)
import Concur.React.Props as Props
import Control.Bind (bind, (>>=))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.Communication.OneTimeShare (share)
import Functions.EnvironmentalVariables (currentCommit, redeemURL)
import Views.ShareView (shareView)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

shareWidget :: String -> Widget HTML Unit
shareWidget secret = do
  version <- liftEffect currentCommit
  do
    (Tuple secret_ password_) <- shareView secret
    result <- liftAff $ runExceptT $ share secret_ password_
    case result of
      Left err -> text ("error:" <> show err)
      Right id -> go id 
    <> p [Props.className "version"] [text version]
  
  where
    go :: String -> Widget HTML Unit
    go id = do
      redeemURL <- liftEffect $ redeemURL
      origin_ <- liftEffect $ window >>= location >>= origin
      _ <- div [Props.className "redeemSecret"] [
        a [Props.href (redeemURL <> id), Props.target "_blank"] [text "Share Link"]
      , button [(copyToClipboard (origin_ <> redeemURL <> id)) <$ Props.onClick] [text "copy"]
      ]
      go id
