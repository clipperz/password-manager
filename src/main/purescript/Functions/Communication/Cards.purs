module Functions.Communication.Cards where

import Affjax.RequestBody (formData)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.Except.Trans (ExceptT, throwError)
import Crypto.Subtle.Constants.AES (aesCTR, l256)
import Crypto.Subtle.Key.Generate as KG
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (decrypt, encrypt, raw, unwrapKey)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HTTP.Method (Method(..))
import Data.HexString (Base(..), toArrayBuffer, toString)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..))
import DataModel.Card (Card)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardReference(..), CardEntry(..), createCardEntry)
import DataModel.SRP (hashFuncSHA256)
import DataModel.StatelessAppState (ProxyResponse(..), StatelessAppState)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Card (getCardContent)
import Functions.CardsCache (addCardToCache, getCardFromCache, removeCardFromCache)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.BlobFromArrayBuffer (blobFromArrayBuffer)
import Functions.Communication.Blobs (deleteStatelessBlob, getBlob, postBlob, postStatelessBlob)
import Functions.Communication.StatelessBackend (ConnectionState)
import Functions.EncodeDecode (cryptoKeyAES, encryptJson)
import Web.XHR.FormData (EntryName(..), FileName(..), appendBlob, new)

getCard :: CardReference -> ExceptT AppError Aff Card
getCard cardRef@(CardReference { reference }) = do
  maybeCard <- getCardFromCache reference
  case maybeCard of
    Just card -> pure $ card
    Nothing -> do
      blob <- getBlob reference
      card <- getCardContent blob cardRef
      addCardToCache reference card
      pure $ card

-- TODO REMOVE
deleteCardWithState :: CardReference -> ExceptT AppError Aff String
deleteCardWithState cardReference@(CardReference { reference, key }) = do
  let url = joinWith "/" ["blobs", show reference]
  card          <- getCard cardReference
  cryptoKey     <- liftAff $ KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  encryptedCard <- liftAff $ encryptJson cryptoKey card
  body          <- formData <$> (liftEffect $ do
    formData <- new
    appendBlob (EntryName "blob") (blobFromArrayBuffer encryptedCard) (Just $ FileName (toString Hex reference)) formData
    pure $ formData
  )
  response <- manageGenericRequest url DELETE (Just body) RF.string
  if isStatusCodeOk response.status
    then do
      removeCardFromCache reference
      pure response.body
    else throwError $ ProtocolError (ResponseError $ unwrap response.status)
-- ------------

deleteCard :: ConnectionState -> CardReference -> Card -> ExceptT AppError Aff (ProxyResponse String)
deleteCard connectionState (CardReference { reference, key }) card = do
  cryptoKey     <- liftAff $ cryptoKeyAES (toArrayBuffer key)
  encryptedCard <- liftAff $ encryptJson cryptoKey card
  deleteStatelessBlob connectionState encryptedCard reference

-- TODO REMOVE
postCardWithState :: Card -> ExceptT AppError Aff CardEntry
postCardWithState card = do
  key <- liftAff $ KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  Tuple encryptedCard cardEntry@(CardEntry {cardReference: CardReference {reference}}) <- liftAff $ createCardEntry card key hashFuncSHA256
  _ <- postBlob encryptedCard (toArrayBuffer reference)
  addCardToCache reference card
  pure cardEntry
-- -----------

postCard :: StatelessAppState -> Card -> ExceptT AppError Aff (ProxyResponse CardEntry)
postCard {proxy, hash} card = do
  key <- liftAff $ KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  Tuple encryptedCard cardEntry@(CardEntry {cardReference: CardReference {reference}}) <- liftAff $ createCardEntry card key hashFuncSHA256
  ProxyResponse proxy' _ <- postStatelessBlob {proxy, hashFunc: hash} encryptedCard (toArrayBuffer reference)
  addCardToCache reference card
  pure $ ProxyResponse proxy' cardEntry



