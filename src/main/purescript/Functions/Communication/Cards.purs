module Functions.Communication.Cards where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT)
import Data.Function (($))
import Data.Map (insert, lookup)
import Data.Map.Internal (delete)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError)
import DataModel.AppState (CardsCache, ProxyResponse(..))
import DataModel.CardVersions.Card (Card)
import DataModel.IndexVersions.Index (CardEntry(..), CardReference(..), reference)
import DataModel.SRPVersions.SRP (hashFuncSHA256)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Card (createCardEntry, decryptCard)
import Functions.Communication.Backend (ConnectionState)
import Functions.Communication.Blobs (deleteBlob, getBlob, postBlob)

getCard :: ConnectionState -> CardsCache -> CardEntry -> ExceptT AppError Aff (ProxyResponse (Tuple CardsCache Card))
getCard connectionState cardsCache cardEntry@(CardEntry entry) = do
  let cardFromCache = lookup (reference cardEntry) cardsCache
  case cardFromCache of
    Just card -> pure $ ProxyResponse connectionState.proxy (Tuple cardsCache card)
    Nothing   -> do
      ProxyResponse proxy blob <- getBlob connectionState (reference cardEntry)
      card                      <- decryptCard blob (entry.cardReference)
      let updatedCardsCache = insert (reference cardEntry) card cardsCache
      pure $ ProxyResponse proxy (Tuple updatedCardsCache card)

deleteCard :: ConnectionState -> CardsCache -> CardReference -> ExceptT AppError Aff (ProxyResponse CardsCache)
deleteCard connectionState cardsCache (CardReference { reference, identifier }) = do
  ProxyResponse proxy _ <- deleteBlob connectionState reference identifier
  let updatedCardsCache =  delete identifier cardsCache
  pure $ ProxyResponse proxy updatedCardsCache

postCard :: ConnectionState -> CardsCache -> Card -> ExceptT AppError Aff (ProxyResponse (Tuple CardsCache CardEntry))
postCard connectionState cardsCache card = do
  Tuple encryptedCard cardEntry@(CardEntry {cardReference: CardReference {reference, identifier}}) <- liftAff $ createCardEntry hashFuncSHA256 card
  ProxyResponse proxy _ <- postBlob connectionState encryptedCard reference identifier
  let updatedCardsCache  = insert reference card cardsCache
  pure $ ProxyResponse proxy (Tuple updatedCardsCache cardEntry)
