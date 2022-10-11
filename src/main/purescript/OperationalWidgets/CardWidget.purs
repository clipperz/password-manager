module OperationalWidgets.CardWidget where

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
import Data.Functor ((<$), (<$>), void)
import Data.Int (ceil)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import DataModel.AppState (AppError)
import DataModel.Card (Card(..), CardValues(..))
import DataModel.Index (CardEntry, CardReference)
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Functions.Communication.Cards (getCard, postCard)
import Views.CardViews (cardView, CardAction(..))
import Views.SimpleWebComponents (loadingDiv)

cardWidget :: CardReference -> WidgetState -> Widget HTML IndexUpdateData
cardWidget reference state = do
  eitherCard <- case state of 
    Error err -> div [] [text $ "Card could't be loaded: " <> err]
    _ -> loadingDiv <|> (liftAff $ runExceptT $ getCard reference)
  case eitherCard of
    Right c -> do 
      res <- cardView c
      manageCardAction res
    Left err -> cardWidget reference (Error (show err))

  where
    manageCardAction :: CardAction -> Widget HTML IndexUpdateData
    manageCardAction action = 
      case action of
        Edit cc -> pure $ IndexUpdateData (ChangeToReference reference) cc
        Clone cc@(Card_v1 cardRecord) -> do
          clonedCard <- liftAff $ cloneCardNow cc
          doOp cc (postCard clonedCard) (\entry -> IndexUpdateData (CloneReference entry) cc)
        Archive cc -> pure $ IndexUpdateData (ChangeToReference reference) cc
        Delete cc -> pure $ IndexUpdateData (DeleteReference reference) cc

    doOp :: forall a. Card -> ExceptT AppError Aff a -> (a -> IndexUpdateData) -> Widget HTML IndexUpdateData
    doOp currentCard op mapResult = do
      res <- (inertCardView currentCard) <|> (liftAff $ runExceptT $ op)
      case res of
        Right a -> pure $ mapResult a
        Left err -> div [] [text ("Current operation could't be completed: " <> show err)
                           , cardView currentCard >>= manageCardAction ]

    inertCardView :: forall a. Card -> Widget HTML a
    inertCardView card = do
      _ <- div [] [
        loadingDiv
      , cardView card -- TODO: need to deactivate buttons to avoid returning some value here
      ]
      loadingDiv

cloneCardNow :: Card -> Aff Card
cloneCardNow card@(Card_v1 { timestamp: t, content}) =
  case content of
    CardValues_v1 values -> do
      timestamp <- liftEffect $ (ceil <<< unwrap <<< unInstant) <$> now
      pure $ Card_v1 { timestamp, content: (CardValues_v1 (values { title = (values.title <> " - CLONE")}))}
    _ -> pure card
