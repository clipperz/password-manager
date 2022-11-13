module OperationalWidgets.CardWidget
  ( cardWidget
  )
  where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM (div, text)
import Control.Alt ((<|>))
import Control.Applicative (pure)
import Control.Bind (bind, (>>=))
import Control.Monad.Except.Trans (runExceptT, ExceptT)
import Data.Either (Either(..))
import Data.Eq ((==))
import Data.Function (($))
import Data.PrettyShow (prettyShow)
import Data.Semigroup ((<>))
import Data.Show (show)
import DataModel.AppState (AppError)
import DataModel.Card (Card(..), CardValues(..))
import DataModel.Index (CardEntry(..))
import DataModel.WidgetOperations (IndexUpdateAction(..), IndexUpdateData(..))
import DataModel.WidgetState (WidgetState(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Functions.Communication.Cards (getCard, postCard, deleteCard)
import Functions.JSState (getAppState)
import Functions.State (isOfflineCopy)
import Functions.Time (getCurrentTimestamp)
import OperationalWidgets.CreateCardWidget (createCardWidget)
import Views.CardViews (cardView, CardAction(..))
import Views.CreateCardView (createCardView)
import Views.SimpleWebComponents (loadingDiv)

cardWidget :: CardEntry -> Array String -> WidgetState -> Widget HTML IndexUpdateData
cardWidget entry@(CardEntry r@{ title: _, cardReference, archived: _, tags: _ }) tags state = do
  eitherState <- liftEffect $ getAppState
  case eitherState of
    Left err -> cardWidget entry tags (Error (prettyShow err))
    Right st -> do
      eitherCard <- case state of 
        Error err -> div [] [text $ "Card could't be loaded: " <> err]
        _ -> loadingDiv <|> (liftAff $ runExceptT $ getCard cardReference)
      case eitherCard of
        Right c -> do 
          res <- cardView c (isOfflineCopy st)
          manageCardAction res (isOfflineCopy st)
        Left err -> do
          _ <- liftEffect $ log $ show err
          cardWidget entry tags (Error (prettyShow err))

  where
    manageCardAction :: CardAction -> Boolean -> Widget HTML IndexUpdateData
    manageCardAction action isOffline = 
      case action of
        Edit cc -> do
          IndexUpdateData indexUpdateAction newCard <- createCardWidget cc tags Default -- here the modified card has already been saved
          case indexUpdateAction of
            AddReference newEntry -> pure $ IndexUpdateData (ChangeReferenceWithEdit entry newEntry) newCard
            _ -> cardWidget entry tags Default
        Used cc -> do
          timestamp' <- liftEffect $ getCurrentTimestamp
          if r.lastUsed == timestamp' then do
            pure $ IndexUpdateData (NoUpdateNecessary entry) cc
          else do
            pure $ IndexUpdateData (ChangeReferenceWithoutEdit entry (CardEntry $ r { lastUsed = timestamp' })) cc
        Clone cc -> do
          clonedCard <- liftAff $ cloneCardNow cc
          doOp isOffline cc cc false (postCard clonedCard) (\newEntry -> IndexUpdateData (CloneReference newEntry) cc)
        Archive oldCard@(Card rc) -> do
          timestamp' <- liftEffect $ getCurrentTimestamp
          let newCard = Card $ rc { timestamp = timestamp', archived = true }
          doOp isOffline oldCard newCard false (postCard newCard) (\newEntry -> IndexUpdateData (ChangeReferenceWithoutEdit entry newEntry) newCard)
        Restore oldCard@(Card rc) -> do
          timestamp' <- liftEffect $ getCurrentTimestamp
          let newCard = Card $ rc { timestamp = timestamp', archived = false }
          doOp isOffline oldCard newCard false (postCard newCard) (\newEntry -> IndexUpdateData (ChangeReferenceWithoutEdit entry newEntry) newCard)
        Delete cc -> doOp isOffline cc cc false (deleteCard cardReference) (\_ -> IndexUpdateData (DeleteReference entry) cc)

    doOp :: forall a. Boolean -> Card -> Card -> Boolean -> ExceptT AppError Aff a -> (a -> IndexUpdateData) -> Widget HTML IndexUpdateData
    doOp isOffline oldCard currentCard showForm op mapResult = do
      res <- (if showForm then inertCardFormView currentCard else inertCardView currentCard) <|> (liftAff $ runExceptT $ op)
      case res of
        Right a -> pure $ mapResult a
        Left err -> do
          _ <- liftEffect $ log $ show err
          div [] [ text ("Current operation could't be completed: " <> prettyShow err)
                           , (cardView oldCard isOffline) >>= (\ca -> manageCardAction ca isOffline) ]

    inertCardView :: forall a. Card -> Widget HTML a
    inertCardView card = do
      _ <- div [] [
        loadingDiv
      , cardView card true -- TODO: need to deactivate buttons to avoid returning some value here
      ]
      loadingDiv

    inertCardFormView :: forall a. Card -> Widget HTML a
    inertCardFormView card = do
      _ <- createCardView card tags Loading -- TODO: need to deactivate buttons to avoid returning some value here
      loadingDiv

cloneCardNow :: Card -> Aff Card
cloneCardNow (Card { timestamp: _, content, archived}) =
  case content of
    CardValues values -> do
      timestamp <- liftEffect $ getCurrentTimestamp
      pure $ Card { timestamp, archived, content: (CardValues (values { title = (values.title <> " - CLONE")}))}
