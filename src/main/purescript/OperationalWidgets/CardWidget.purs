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
import Views.CardViews (cardView, CardAction(..), createCardView)
import Views.SimpleWebComponents (loadingDiv)

data IndexUpdateAction = AddReference Card CardEntry | DeleteReference Card | ChangeToReference Card Card | NoUpdate
instance showIndexUpdateAction :: Show IndexUpdateAction where
  show (AddReference c _) = "Add reference to " <> show c
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
          timestamp <- liftEffect $ (ceil <<< unwrap <<< unInstant) <$> now
          doOp cc (postCard (Card_v1 $ cardRecord { timestamp = timestamp })) (AddReference cc)
        Archive cc -> pure $ ChangeToReference cc cc
        Delete cc -> pure $ DeleteReference cc

    doOp :: forall a. Card -> ExceptT AppError Aff a -> (a -> IndexUpdateAction) -> Widget HTML IndexUpdateAction
    doOp currentCard op mapResult = do
      res <- loadingDiv <|> (liftAff $ runExceptT $ op)
      case res of
        Right a -> pure $ mapResult a
        Left err -> div [] [text ("Current operation could't be completed: " <> show err)
                           , cardView currentCard >>= manageCardAction ]

data CreateCardActions = JustCard Card | EitherReference (Either AppError CardEntry)

createCardWidget :: Card -> Widget HTML IndexUpdateAction
createCardWidget startingCard = go Default startingCard
  where 
    go :: WidgetState -> Card -> Widget HTML IndexUpdateAction
    go state c = do
      res <- case state of
        Default -> JustCard <$> (createCardView c)
        Loading -> loadingDiv <|> (EitherReference <$> (liftAff $ runExceptT $ postCard c)) -- TODO: draw loadingDiv over form
        Error err -> div [] [text $ "Card could't be saved: " <> err, JustCard <$> (createCardView c)]
      case res of
        -- Right ref -> pure $ AddReference ref
        JustCard card -> go Loading card
        EitherReference e -> case e of
          Right entry -> pure $ AddReference c entry
          Left err -> go (Error (show err)) c
