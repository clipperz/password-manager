module OperationalWidgets.ShareWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, p, text)
import Concur.React.Props as Props
import Control.Alt ((<|>))
import Control.Alternative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit, unit)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.Communication.OneTimeShare (SecretData, share)
import Functions.EnvironmentalVariables (currentCommit, redeemURL)
import Views.OverlayView (OverlayStatus(..), overlay)
import Views.ShareView (Secret, emptySecretData, shareView)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

shareWidget :: Secret -> Widget HTML Unit
shareWidget secret = do
  version <- liftEffect currentCommit
  do
    secretData <- shareView true emptySecretData secret
    result <- liftAff $ runExceptT $ share secretData
    case result of
      Left err -> text ("error:" <> show err)
      Right id -> do
        redeemURL_ <- liftEffect $ redeemURL
        origin_  <- liftEffect $ window >>= location >>= origin
        go (origin_ <> redeemURL_ <> id) secretData
    <> p [Props.className "version"] [text version]
  
  where
    go :: String -> SecretData -> Widget HTML Unit
    go url secretData = do  
      result <- div [Props.className "redeemSecret"] [
                  false <$ shareView false secretData secret
                , div [Props.className "url"] [a [Props.href url, Props.target "_blank"] [text url]]
                , true  <$ button [(\_ -> copyToClipboard url) <$> Props.onClick] [text "copy"]
                ] 
      _ <- if result then 
                go url secretData <|> (liftAff $ delay (Milliseconds 500.0)) <|> overlay { status: Copy, message: "copied" }
           else
                pure unit
      go url secretData
