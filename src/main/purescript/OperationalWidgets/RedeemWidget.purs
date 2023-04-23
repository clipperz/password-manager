module OperationalWidgets.RedeemWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (text)
import Control.Bind (bind)
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import DataModel.AppState (AppError)
import Effect.Aff.Class (liftAff)
import Functions.Communication.OneTimeShare (redeem)
import Views.RedeemView (redeemView)

redeemWidget :: String -> Widget HTML Unit
redeemWidget id = do
  password <- redeemView
  eitherSecret :: Either AppError String <- liftAff $ runExceptT $ redeem id password
  case eitherSecret of
    Right secret -> text ("Here is the secret: " <> secret)
    Left err -> text $ show err