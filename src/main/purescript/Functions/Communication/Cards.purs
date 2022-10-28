module Functions.Communication.Cards where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, except)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Constants.AES (aesCTR, l256)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Generate as KG
import Crypto.Subtle.Key.Types (encrypt, exportKey, decrypt, raw, unwrapKey, CryptoKey)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..), note)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (toArrayBuffer, fromArrayBuffer)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit)
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Card (Card)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardReference(..), Index, CardEntry(..), createCardEntry)
import DataModel.SRP (hashFuncSHA256)
import DataModel.User (UserCard(..), IndexReference(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Functions.Card (getCardContent)
import Functions.CardsCache (getCardFromCache, addCardToCache)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.Blobs (postBlob, getBlob, deleteBlob)
import Functions.EncodeDecode (encryptJson)
import Functions.Index (getIndexContent)
import Functions.JSState (getAppState, modifyAppState)
import Functions.State (getHashFromState)

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
  -- deleteBlob reference
  let url = joinWith "/" ["blobs", show reference]
  card <- getCard cardReference
  cryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
  encryptedCard <- ExceptT $ Right <$> encryptJson cryptoKey card
  let body = json $ encodeJson $ { data: fromArrayBuffer encryptedCard, hash: reference } :: RequestBody
  response <- manageGenericRequest url DELETE (Just body) RF.string
  if isStatusCodeOk response.status
    then except $ Right $ response.body
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

postCard :: Card -> ExceptT AppError Aff CardEntry
postCard card = do
  key <- ExceptT $ Right <$> (KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey])
  _ <- ExceptT $ Right <$> (fromArrayBuffer <$> exportKey raw key)
  Tuple encryptedCard cardEntry <- ExceptT $ Right <$> (createCardEntry card key hashFuncSHA256)
  case cardEntry of
    CardEntry { title: _
                 , cardReference: (CardReference { reference, key: _ })
                 , archived: _
                 , tags: _
                 } -> do
      _ <- postBlob encryptedCard (toArrayBuffer reference)
      addCardToCache reference card
      pure cardEntry

getUserCard :: ExceptT AppError Aff UserCard
getUserCard = do
  { proxy: _, c: mc, p: _, sessionKey: _, toll: _ } <- ExceptT $ liftEffect $ getAppState
  c <- except $ note (InvalidStateError (MissingValue "c is Nothing")) mc
  let url = joinWith "/" ["users", show c]
  response <- manageGenericRequest url GET Nothing RF.json
  if isStatusCodeOk response.status
    then withExceptT (\e -> ProtocolError (DecodeError (show e))) (except $ decodeJson response.body)
    else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

getIndex :: ExceptT AppError Aff Index
getIndex = do 
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { indexReference: Just indexRef@(IndexReference { reference }) } -> do
      blob <- getBlob reference
      getIndexContent blob indexRef
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing index reference"

updateIndex :: Index -> ExceptT AppError Aff Unit
updateIndex newIndex = do
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: Just c, p: Just p, indexReference: Just (IndexReference oldReference) } -> do
      UserCard userCard <- getUserCard
      masterPassword :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      cryptoKey            :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer oldReference.masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      indexCardContent     :: ArrayBuffer <- ExceptT $ Right <$> encryptJson cryptoKey newIndex
      indexCardContentHash :: ArrayBuffer <- ExceptT $ Right <$> (getHashFromState $ currentState.hash) (indexCardContent : Nil)
      let newIndexReference = IndexReference $ oldReference { reference = fromArrayBuffer indexCardContentHash }
      masterKeyContent <- ExceptT $ (fromArrayBuffer >>> Right) <$> encryptJson masterPassword newIndexReference
      let newUserCard = UserCard $ userCard { masterKeyContent = masterKeyContent }
      _ <- postBlob indexCardContent indexCardContentHash
      let url = joinWith "/" ["users", show c]
      let body = (json $ encodeJson newUserCard) :: RequestBody
      _ <- manageGenericRequest url PUT (Just body) RF.string
      _ <- deleteBlob oldReference.reference -- TODO: manage errors
      ExceptT $ Right <$> (modifyAppState $ currentState { indexReference = Just newIndexReference})
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing p, c or indexReference"
