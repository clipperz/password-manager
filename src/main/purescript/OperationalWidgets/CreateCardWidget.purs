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
import Data.Maybe (maybe, Maybe(..))
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError)
import DataModel.Card (Card(..))
import DataModel.Index (CardEntry, CardReference)
import DataModel.WidgetOperations (IndexUpdateAction(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Functions.Communication.Cards (getCard, postCard)
import Views.CardViews (cardView, CardAction(..))
import Views.CreateCardView (createCardView)
import Views.SimpleWebComponents (loadingDiv)

data CreateCardActions = JustCard Card | EitherReference (Either AppError CardEntry) | NoAction

createCardWidget :: Card -> WidgetState -> Widget HTML (Maybe (Tuple Card CardEntry))
createCardWidget startingCard state = do
  res <- case state of
    Default -> (maybe NoAction JustCard) <$> (createCardView startingCard Default)
    Loading -> ((maybe NoAction JustCard) <$> (createCardView startingCard Loading)) <|> (EitherReference <$> (liftAff $ runExceptT $ postCard startingCard)) -- TODO: draw loadingDiv over form
    Error err -> (maybe NoAction JustCard) <$> (createCardView startingCard (Error ("Card could't be saved: " <> err)))
  case res of
    NoAction -> pure $ Nothing
    JustCard card -> createCardWidget card Loading
    EitherReference e -> case e of
      Right entry -> pure $ Just $ Tuple startingCard entry
      Left err -> createCardWidget startingCard (Error (show err))
