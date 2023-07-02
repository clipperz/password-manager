module OperationalWidgets.ShareWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, text)
import Concur.React.Props as Props
import Control.Bind (bind)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>), (<$))
import Data.Semigroup ((<>))
import Data.Show (show)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.Communication.OneTimeShare (share)
import Functions.EnvironmentalVariables (redeemURL)
import Views.ShareView (Secret, shareView)

shareWidget :: Secret -> Widget HTML Boolean
shareWidget dataSecret = do
  secretInfo <- shareView dataSecret
  result <- liftAff $ runExceptT $ share secretInfo
  case result of
    Left err -> text ("error:" <> show err)
    Right id -> do
      redeemURLOrigin <- liftEffect $ redeemURL
      let redeemURL = redeemURLOrigin <> id
      div [Props.className "redeemSecret"] [
        text "Redeem Secret:"
      , a [Props.href redeemURL, Props.target "_blank"] [text redeemURL]
      , true <$ button [(\_ -> copyToClipboard redeemURL) <$> Props.onClick] [text "Copy"]
      ] 
