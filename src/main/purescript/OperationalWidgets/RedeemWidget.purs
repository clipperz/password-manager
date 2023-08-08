module OperationalWidgets.RedeemWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (p, text)
import Concur.React.Props as Props
import Control.Bind (bind)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import DataModel.AppState (AppError(..))
import DataModel.Communication.ProtocolError (ProtocolError(..))
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Communication.OneTimeShare (redeem)
import Functions.EnvironmentalVariables (currentCommit)
import Views.RedeemView (redeemView, redeemedView)

redeemWidget :: String -> String -> Widget HTML Unit
redeemWidget id key = do
  version <- liftEffect currentCommit
  do
    pin <- redeemView
    eitherSecret :: Either AppError String <- liftAff $ runExceptT $ redeem id key pin
    case eitherSecret of
      Right secret -> redeemedView secret
      Left err -> case err of
        ProtocolError (ResponseError 404) -> text $ "Secret already redeemed"
        ProtocolError (ResponseError 410) -> text $ "Secret expired"
        _                                 -> text $ show err
    <> p [Props.className "version"] [text version]