module OperationalWidgets.CardWidget where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT)
import Control.Semigroupoid ((<<<), (>>>))
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
import DataModel.Card (Card(..), CardValues(..))
import DataModel.Index (CardEntry, CardReference)
import DataModel.WidgetOperations (IndexUpdateAction(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Now (now)
import Functions.Communication.Cards (getCard, postCard, deleteCard)
import Views.CardViews (cardView, CardAction(..))
import Views.CreateCardView (createCardView)
import Views.SimpleWebComponents (loadingDiv)
import OperationalWidgets.CreateCardWidget (createCardWidget)

cardWidget :: CardReference -> WidgetState -> Widget HTML IndexUpdateAction -- TODO: is widgetstate necessary?
cardWidget reference state = do
  eitherCard <- case state of 
    Error err -> div [] [text $ "Card could't be loaded: " <> err]
    _ -> loadingDiv <|> (liftAff $ runExceptT $ getCard reference)
  case eitherCard of
    Right c -> do 
      res <- ({ reference: reference, action: _ }) <$> (cardView c)
      manageCardAction res
    Left err -> do
      -- TODO: check error to decide what to do
      NoUpdate <$ div [] [text $ show err]

  where
    manageCardAction :: { reference :: CardReference, action :: CardAction} -> Widget HTML IndexUpdateAction
    manageCardAction {reference, action} = 
      case action of
        Edit cc -> do
          createResult <- createCardWidget cc Default
          case createResult of
            Just (Tuple newCard newEntry) -> do
              _ <- liftEffect $ log $ "Card added: " <> show newCard
              doOp newCard reference (deleteCard reference) (\_ -> ChangeToReference cc reference newEntry)
            Nothing -> cardWidget reference Default
        Clone cc@(Card_v1 cardRecord) -> do
          clonedCard <- liftAff $ cloneCardNow cc
          doOp cc reference (postCard clonedCard) CloneReference
        Archive cc -> pure $ DeleteReference reference -- TODO:
        Delete cc -> pure $ DeleteReference reference

    doOp :: forall a. Card 
         -> CardReference
         -> ExceptT AppError Aff a 
         -> (a -> IndexUpdateAction) 
         -> Widget HTML IndexUpdateAction
    doOp currentCard currentReference op mapResult = do
      res <- loadingDiv <|> (liftAff $ runExceptT $ op)
      case res of
        Right a -> pure $ mapResult a
        Left err -> div [] [text ("Current operation could't be completed: " <> show err)
                           , cardView currentCard >>= (({ action: _, reference}) >>> manageCardAction) ]

cloneCardNow :: Card -> Aff Card
cloneCardNow card@(Card_v1 { timestamp: t, content}) =
  case content of
    CardValues_v1 values -> do
      timestamp <- liftEffect $ (ceil <<< unwrap <<< unInstant) <$> now
      pure $ Card_v1 { timestamp, content: (CardValues_v1 (values { title = (values.title <> " - CLONE")}))}
    _ -> pure card
