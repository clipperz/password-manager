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
import Data.Functor ((<$), (<$>))
import Data.Int (ceil)
import Data.Maybe (maybe)
import Data.Newtype (unwrap)
import Data.Semigroup ((<>))
import Data.Show (show, class Show)
import DataModel.AppState (AppError)
import DataModel.Card (Card(..), CardValues(..))
import DataModel.Index (CardEntry, CardReference)
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Functions.Communication.Cards (getCard, postCard)
import Views.CardViews (cardView, CardAction(..))
import Views.SimpleWebComponents (loadingDiv)

data IndexUpdateAction = AddReference Card CardEntry | CloneReference CardEntry | DeleteReference Card | ChangeToReference Card Card | NoUpdate
instance showIndexUpdateAction :: Show IndexUpdateAction where
  show (AddReference c _) = "Add reference to " <> show c
  show (CloneReference c ) = "Clone reference to " <> show c
  show (DeleteReference c) = "Delete reference to " <> show c
  show (ChangeToReference c c') = "Change reference of " <> show c <> " to " <> show c'
  show NoUpdate = "No update"

cardWidget :: CardReference -> Widget HTML IndexUpdateAction
cardWidget reference = go Loading
  where 
    go state = do
      eitherCard <- case state of 
        Default -> div [] []
        Loading -> loadingDiv <|> (liftAff $ runExceptT $ getCard reference)
        Error err -> div [] [text $ "Card could't be loaded: " <> err]
      case eitherCard of
        Right c -> do 
          res <- cardView c
          manageCardAction res
        Left err -> do
          -- TODO: check error to decide what to do
          NoUpdate <$ div [] [text $ show err]

    manageCardAction :: CardAction -> Widget HTML IndexUpdateAction
    manageCardAction action = 
      case action of
        Edit cc -> pure $ ChangeToReference cc cc
        Clone cc@(Card_v1 cardRecord) -> do
          clonedCard <- liftAff $ cloneCardNow cc
          doOp cc (postCard clonedCard) CloneReference
        Archive cc -> pure $ ChangeToReference cc cc
        Delete cc -> pure $ DeleteReference cc

    doOp :: forall a. Card -> ExceptT AppError Aff a -> (a -> IndexUpdateAction) -> Widget HTML IndexUpdateAction
    doOp currentCard op mapResult = do
      res <- loadingDiv <|> (liftAff $ runExceptT $ op)
      case res of
        Right a -> pure $ mapResult a
        Left err -> div [] [text ("Current operation could't be completed: " <> show err)
                           , cardView currentCard >>= manageCardAction ]

cloneCardNow :: Card -> Aff Card
cloneCardNow card@(Card_v1 { timestamp: t, content}) =
  case content of
    CardValues_v1 values -> do
      timestamp <- liftEffect $ (ceil <<< unwrap <<< unInstant) <$> now
      pure $ Card_v1 { timestamp, content: (CardValues_v1 (values { title = (values.title <> " - CLONE")}))}
    _ -> pure card
