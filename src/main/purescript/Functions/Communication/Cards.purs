module Functions.Communication.Cards where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT)
import Data.Function (($))
import Data.HexString (toArrayBuffer)
import Data.Map (insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError)
import DataModel.AppState (CardsCache, ProxyResponse(..))
import DataModel.Card (Card)
import DataModel.Codec as Codec
import DataModel.Index (CardEntry(..), CardReference(..), reference)
import DataModel.SRP (hashFuncSHA256)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Card (getCardContent)
import Functions.Communication.Backend (ConnectionState)
import Functions.Communication.Blobs (deleteBlob, getBlob, postBlob)
import Functions.EncodeDecode (encryptJson, generateCryptoKeyAesGCM, importCryptoKeyAesGCM)
import Functions.Index (createCardEntry)

getCard :: ConnectionState -> CardsCache -> CardEntry -> ExceptT AppError Aff (ProxyResponse (Tuple CardsCache Card))
getCard connectionState cardsCache cardEntry@(CardEntry entry) = do
  let cardFromCache = lookup (reference cardEntry) cardsCache
  case cardFromCache of
    Just card -> pure $ ProxyResponse connectionState.proxy (Tuple cardsCache card)
    Nothing   -> do
      ProxyResponse proxy' blob <- getBlob connectionState (reference cardEntry)
      card                      <- getCardContent blob (entry.cardReference)
      let updatedCardsCache = insert (reference cardEntry) card cardsCache
      pure $ ProxyResponse proxy' (Tuple updatedCardsCache card)

deleteCard :: ConnectionState -> CardReference -> Card -> ExceptT AppError Aff (ProxyResponse String)
deleteCard connectionState (CardReference { reference, key }) card = do
  cryptoKey     <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer key)
  encryptedCard <- liftAff $ encryptJson Codec.cardCodec cryptoKey card
  deleteBlob connectionState encryptedCard reference

postCard :: ConnectionState -> Card -> ExceptT AppError Aff (ProxyResponse CardEntry)
postCard connectionState card = do
  key <- liftAff $ generateCryptoKeyAesGCM
  Tuple encryptedCard cardEntry@(CardEntry {cardReference: CardReference {reference}}) <- liftAff $ createCardEntry card key hashFuncSHA256
  ProxyResponse proxy' _ <- postBlob connectionState encryptedCard (toArrayBuffer reference)
  pure $ ProxyResponse proxy' cardEntry

