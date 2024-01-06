module Functions.Communication.Cards where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Except.Trans (ExceptT)
import Crypto.Subtle.Constants.AES (aesCTR, l256)
import Crypto.Subtle.Key.Generate as KG
import Crypto.Subtle.Key.Types (decrypt, encrypt, unwrapKey)
import Data.Function (($))
import Data.HexString (toArrayBuffer)
import Data.Map (insert, lookup)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError)
import DataModel.Card (Card)
import DataModel.Index (CardEntry(..), CardReference(..), createCardEntry, reference)
import DataModel.SRP (hashFuncSHA256)
import DataModel.AppState (ProxyResponse(..), AppState)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.Card (getCardContent)
import Functions.Communication.Blobs (deleteBlob, getBlob, postBlob)
import Functions.Communication.Backend (ConnectionState)
import Functions.EncodeDecode (cryptoKeyAES, encryptJson)

getCard :: AppState -> CardEntry -> ExceptT AppError Aff (Tuple AppState Card)
getCard state@{proxy, hash, cardsCache} cardEntry@(CardEntry entry) = do
  let cardFromCache = lookup (reference cardEntry) cardsCache
  case cardFromCache of
    Just card -> pure $ Tuple state card
    Nothing   -> do
      ProxyResponse proxy' blob <- getBlob {proxy, hashFunc: hash} (reference cardEntry)
      card                      <- getCardContent blob (entry.cardReference)
      let updatedCardsCache = insert (reference cardEntry) card cardsCache
      pure $ Tuple
        (state { proxy      = proxy'
               , cardsCache = updatedCardsCache})
         card

deleteCard :: ConnectionState -> CardReference -> Card -> ExceptT AppError Aff (ProxyResponse String)
deleteCard connectionState (CardReference { reference, key }) card = do
  cryptoKey     <- liftAff $ cryptoKeyAES (toArrayBuffer key)
  encryptedCard <- liftAff $ encryptJson cryptoKey card
  deleteBlob connectionState encryptedCard reference

postCard :: AppState -> Card -> ExceptT AppError Aff (ProxyResponse CardEntry)
postCard {proxy, hash} card = do
  key <- liftAff $ KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  Tuple encryptedCard cardEntry@(CardEntry {cardReference: CardReference {reference}}) <- liftAff $ createCardEntry card key hashFuncSHA256
  ProxyResponse proxy' _ <- postBlob {proxy, hashFunc: hash} encryptedCard (toArrayBuffer reference)
  pure $ ProxyResponse proxy' cardEntry

