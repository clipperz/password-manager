module OperationalWidgets.RedeemWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, loopW)
import Concur.React (HTML)
import Concur.React.DOM (button, div, text)
import Concur.React.Props as Props
import Control.Bind (bind, (<$))
import Control.Monad.Except (runExceptT)
import Data.Argonaut.Decode (fromJsonString)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit, unit)
import DataModel.AppState (AppError(..))
import DataModel.Card (Card(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff.Class (liftAff)
import Functions.Clipboard (copyToClipboard)
import Functions.Communication.OneTimeShare (redeem)
import Views.CardViews (cardContent)
import Views.RedeemView (redeemView)

redeemWidget :: String -> Widget HTML Unit
redeemWidget id = do
  password <- redeemView
  eitherSecret :: Either AppError String <- liftAff $ runExceptT $ redeem id password
  case eitherSecret of
    Right secret -> demand $ Nothing <$ loopW unit (\_ ->
      case fromJsonString secret of
        Right (Card {content}) -> unit <$ div [] [
          text ("Here is your secret card:")
        , cardContent content
        , button [(copyToClipboard secret) <$ Props.onClick] [ text "Copy to clipboard"]
        ]
        Left _                 -> text ("Here is the secret: " <> secret)
    )
    Left err -> case err of
      ProtocolError (ResponseError 404) -> text $ "Secret already redeemed"
      _                                 -> text $ show err