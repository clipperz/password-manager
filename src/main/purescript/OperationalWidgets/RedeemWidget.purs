module OperationalWidgets.RedeemWidget where

import Concur.Core (Widget)
import Concur.Core.FRP (demand, loopW)
import Concur.React (HTML)
import Concur.React.DOM (a, button, div, p, text)
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
import Effect.Class (liftEffect)
import Functions.Clipboard (copyToClipboard)
import Functions.Communication.OneTimeShare (redeem)
import Functions.EnvironmentalVariables (appURL, currentCommit)
import Views.CardViews (cardContent)
import Views.RedeemView (redeemView)

redeemWidget :: String -> Widget HTML Unit
redeemWidget id = do
  version <- liftEffect currentCommit
  do
    password <- redeemView
    eitherSecret :: Either AppError String <- liftAff $ runExceptT $ redeem id password
    case eitherSecret of
      Right secret -> demand $ Nothing <$ loopW unit (\_ ->
        case fromJsonString secret of
          Right (Card {content}) -> unit <$ div [] [
            text ("Here is your secret card:")
          , cardContent content
          , button [(copyToClipboard secret) <$ Props.onClick] [ text "Copy to clipboard"]
          , do
              appURL <- liftEffect $ appURL
              button [Props.className "addCardToAccount"] [
                a [Props.href (appURL <> "#addCard?" <> secret), Props.target "_blank"] [
                  text "add to account"
                ]
              ]
          ]
          Left _                 -> text ("Here is the secret: " <> secret)
      )
      Left err -> case err of
        ProtocolError (ResponseError 404) -> text $ "Secret already redeemed"
        ProtocolError (ResponseError 410) -> text $ "Secret expired"
        _                                 -> text $ show err
    <> p [Props.className "version"] [text version]