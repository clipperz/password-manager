module OperationalWidgets.ShareWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, p, text)
import Concur.React.Props as Props
import Control.Alt (($>), (<|>))
import Control.Alternative (pure, (*>))
import Control.Bind (bind, (=<<), (>>=))
import Control.Monad.Except (runExceptT)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$))
import Data.HexString (fromArrayBuffer, toString)
import Data.HexString as Base
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import DataModel.OneTimeShare (SecretData)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.Communication.Backend (ConnectionState)
import Functions.Communication.OneTimeShare (encryptKeyWithPin, encryptSecret, share)
import Functions.EnvironmentalVariables (currentCommit, redeemURL)
import Views.OverlayView (OverlayColor(..), OverlayStatus(..), overlay)
import Views.ShareView (Secret, emptySecretData, shareView)
import Web.HTML (window)
import Web.HTML.Location (origin)
import Web.HTML.Window (location)

shareWidget :: ConnectionState -> Secret -> Widget HTML Unit
shareWidget connectionState secret = do
  version <- liftEffect currentCommit
  do
    secretData <- shareView true secret =<< liftAff emptySecretData
    Tuple encryptionKey encryptedSecret <- liftAff $ encryptSecret CA.string secretData.secret
    result <- ( liftAff $ runExceptT $ share connectionState encryptedSecret secretData.duration )
              <|>
              ( overlay { status: Spinner, color: Black, message: "loading" } )
              <|> 
              ( Right "" <$ shareView false secret secretData )
    case result of
      Left  err -> text ("error:" <> show err)
      Right uuid -> do
        encryptedKey <- liftAff $ encryptKeyWithPin encryptionKey secretData.pin
        redeemURL_ <- liftEffect $ redeemURL
        origin_  <- liftEffect $ window >>= location >>= origin
        go (origin_ <> redeemURL_ <> uuid <> "#" <> (toString Base.Hex $ fromArrayBuffer encryptedKey)) secretData
    <> p [Props.className "version"] [text version]
  
  where
    go :: String -> SecretData -> Widget HTML Unit
    go url secretData = do  
      result <- (false <$ shareView false secret secretData)
                <>
                div [Props.className "share"] [
                  div [Props.className "url"] [a [Props.href url, Props.target "_blank"] [text url]]
                , button [Props.onClick] [text "copy share link"] *> (liftAff $ copyToClipboard url $> true)
                ]
      _ <- if result then 
                go url secretData <|> (liftAff $ delay (Milliseconds 1000.0)) <|> overlay { status: Copy, color: Black, message: "copied" }
           else
                pure unit
      go url secretData
