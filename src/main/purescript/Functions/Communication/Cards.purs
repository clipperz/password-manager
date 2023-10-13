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
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Functions.Card (getCardContent)
import Functions.CardsCache (addCardToCache, getCardFromCache, removeCardFromCache)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.BlobFromArrayBuffer (blobFromArrayBuffer)
import Functions.Communication.Blobs (postBlob, getBlob)
import Functions.EncodeDecode (encryptJson)
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

deleteCard :: CardReference -> ExceptT AppError Aff String
deleteCard cardReference@(CardReference { reference, key }) = do
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

postCard :: Card -> ExceptT AppError Aff CardEntry
postCard card = do
  key <- liftAff $ KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey]
  Tuple encryptedCard cardEntry <- liftAff $ createCardEntry card key hashFuncSHA256
  case cardEntry of
    CardEntry { title: _
              , cardReference: (CardReference { reference, key: _ })
              , archived: _
              , tags: _
              } -> do
      _ <- postBlob encryptedCard (toArrayBuffer reference)
      addCardToCache reference card
      pure cardEntry
