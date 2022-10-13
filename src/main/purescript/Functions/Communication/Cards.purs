module Functions.Communication.Cards where

import Affjax.RequestBody (RequestBody, json)
import Affjax.ResponseFormat as RF
import Control.Applicative (pure)
import Control.Bind (discard, bind, (>>=))
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
import Data.HexString (HexString, toArrayBuffer, fromArrayBuffer, splitHexInHalf)
import Data.HTTP.Method (Method(..))
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Show (show)
import Data.String.Common (joinWith)
import Data.Tuple (Tuple(..))
import DataModel.AppState (AppError(..), InvalidStateError(..))
import DataModel.Card (Card, UserCard)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardReference(..), Index, CardEntry(..), createCardEntry, IndexReference)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EX
import Functions.ArrayBuffer (concatArrayBuffers)
import Functions.CardsCache (getCardFromCache, addCardToCache)
import Functions.Communication.BackendCommunication (isStatusCodeOk, manageGenericRequest)
import Functions.Communication.Blobs (getDecryptedBlob, postBlob)
import Functions.EncodeDecode (encryptArrayBuffer, decryptArrayBuffer, encryptJson)
import Functions.JSState (getAppState)
import Functions.SRP as SRP

getCard :: CardReference -> ExceptT AppError Aff Card
getCard (CardReference_v1 { reference, key }) = do
  maybeCard <- getCardFromCache reference
  case maybeCard of
    Just card -> pure $ card
    Nothing -> do
      cryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer key) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      card <- getDecryptedBlob reference cryptoKey
      addCardToCache reference card
      pure $ card

deleteCard :: CardReference -> ExceptT AppError Aff String
deleteCard reference = 
  let url = joinWith "/" ["blobs", show reference]
  in do
    response <- manageGenericRequest url DELETE Nothing RF.string
    if isStatusCodeOk response.status
      then except $ Right $ response.body
      else except $ Left $ ProtocolError $ ResponseError $ unwrap response.status

postCard :: Card -> ExceptT AppError Aff CardEntry
postCard card = do
  key <- ExceptT $ Right <$> (KG.generateKey (KG.aes aesCTR l256) true [encrypt, decrypt, unwrapKey])
  _ <- ExceptT $ Right <$> (fromArrayBuffer <$> exportKey raw key)
  Tuple encryptedCard cardEntry <- ExceptT $ Right <$> (createCardEntry card key SRP.hashFuncSHA256)
  case cardEntry of
    CardEntry_v1 { title: _
                 , cardReference: (CardReference_v1 { reference, key: _ })
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

getIndex :: HexString -> ExceptT AppError Aff Index
getIndex encryptedRef = do 
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: _, p: Just p, proxy: _, sessionKey: _, toll: _ } -> do
      masterPassword :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      { before: masterKey, after: indexReference } <- mapCryptoError $ ExceptT $ splitInHalf <$> (decryptEncryptedRef masterPassword)
      cryptoKey      :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      getDecryptedBlob indexReference cryptoKey
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing p"
  
  where 
    decryptEncryptedRef :: CryptoKey -> Aff (Either EX.Error ArrayBuffer)
    decryptEncryptedRef password = decryptArrayBuffer password (toArrayBuffer encryptedRef)
    splitInHalf :: Either EX.Error ArrayBuffer -> Either EX.Error { before :: HexString, after :: HexString }
    splitInHalf either = (fromArrayBuffer >>> splitHexInHalf) <$> either
    mapCryptoError :: ExceptT EX.Error Aff { before :: HexString, after :: HexString } -> ExceptT AppError Aff { before :: HexString, after :: HexString }
    mapCryptoError = withExceptT (\e -> ProtocolError $ CryptoError $ EX.message e)

updateIndex :: SRP.SRPConf -> Index -> ExceptT AppError Aff IndexReference
updateIndex conf newIndex = do
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: Just c, p: Just p, proxy: _, sessionKey: _, toll: _ } -> do
      -- create user card with new index reference
      userCard <- getUserCard
      masterPassword :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      { before: masterKey, after: _ } <- mapCryptoError $ ExceptT $ splitInHalf <$> (decryptEncryptedRef masterPassword userCard.masterKeyContent)
      cryptoKey            :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      indexCardContent     :: ArrayBuffer <- ExceptT $ Right <$> encryptJson cryptoKey newIndex
      indexCardContentHash :: ArrayBuffer <- ExceptT $ Right <$> conf.hash (indexCardContent : Nil)
      masterKeyContent     :: ArrayBuffer <- ExceptT $ Right <$> ((liftEffect $ concatArrayBuffers ((toArrayBuffer masterKey) : indexCardContentHash : Nil)) >>= (encryptArrayBuffer masterPassword)) 
      let newUserCard = userCard { masterKeyContent = fromArrayBuffer masterKeyContent }
      -- save new index card
      _ <- postBlob indexCardContent indexCardContentHash
      -- save new user card
      let url = joinWith "/" ["users", show c]
      let body = (json $ encodeJson newUserCard) :: RequestBody
      _ <- manageGenericRequest url PUT (Just body) RF.string
      pure newUserCard.masterKeyContent
    _ -> except $ Left $ InvalidStateError $ MissingValue "Missing p or c"

    
  where 
    decryptEncryptedRef :: CryptoKey -> HexString -> Aff (Either EX.Error ArrayBuffer)
    decryptEncryptedRef password encryptedRef = decryptArrayBuffer password (toArrayBuffer encryptedRef)
    splitInHalf :: Either EX.Error ArrayBuffer -> Either EX.Error { before :: HexString, after :: HexString }
    splitInHalf either = (fromArrayBuffer >>> splitHexInHalf) <$> either
    mapCryptoError :: ExceptT EX.Error Aff { before :: HexString, after :: HexString } -> ExceptT AppError Aff { before :: HexString, after :: HexString }
    mapCryptoError = withExceptT (\e -> ProtocolError $ CryptoError $ EX.message e)

