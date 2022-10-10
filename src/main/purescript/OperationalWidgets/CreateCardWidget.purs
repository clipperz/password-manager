module OperationalWidgets.CreateCardWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT)
import Control.Semigroupoid ((<<<))
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$), (<$>))
import Data.Int (ceil)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import DataModel.AppState (AppError)
import DataModel.Card (Card(..))
import DataModel.Index (CardEntry, CardReference)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Functions.Communication.Cards (getCard, postCard)
import Views.CardViews (cardView, CardAction(..))
import Views.CreateCardView (createCardView)
import Views.SimpleWebComponents (loadingDiv)
import OperationalWidgets.CardWidget (IndexUpdateAction(..))

data CreateCardActions = JustCard Card | EitherReference (Either AppError CardEntry) | NoAction

createCardWidget :: Card -> Widget HTML IndexUpdateAction
createCardWidget startingCard = go Default startingCard
  where 
    go :: WidgetState -> Card -> Widget HTML IndexUpdateAction
    go state c = do
      res <- case state of
        Default -> (maybe NoAction JustCard) <$> (createCardView c)
        Loading -> loadingDiv <|> (EitherReference <$> (liftAff $ runExceptT $ postCard c)) -- TODO: draw loadingDiv over form
        Error err -> div [] [text $ "Card could't be saved: " <> err, (maybe NoAction JustCard) <$> (createCardView c)]
      case res of
        NoAction -> pure $ NoUpdate
        JustCard card -> go Loading card
        EitherReference e -> case e of
          Right entry -> pure $ AddReference c entry
          Left err -> go (Error (show err)) c
