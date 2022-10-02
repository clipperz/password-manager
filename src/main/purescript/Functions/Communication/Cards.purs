module Functions.Communication.Cards where

import Control.Applicative (pure)
import Control.Bind (discard, bind)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, except)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Constants.AES (aesCTR)
import Crypto.Subtle.Key.Import as KI
import Crypto.Subtle.Key.Types (encrypt, decrypt, raw, unwrapKey, CryptoKey)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Either (Either(..))
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HexString (HexString, toArrayBuffer, fromArrayBuffer, splitHexInHalf)
import Data.Maybe (Maybe(..))
import DataModel.AppState (AppError(..))
import DataModel.Card (Card)
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardReference(..), Index)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Exception as EX
import Functions.CardsCache (getCardFromCache, addCardToCache)
import Functions.Communication.Blobs (getDecryptedBlob)
import Functions.EncodeDecode (decryptArrayBuffer)
import Functions.JSState (getAppState)

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

getIndex :: HexString -> ExceptT AppError Aff Index
getIndex encryptedRef = do 
  currentState <- ExceptT $ liftEffect getAppState
  case currentState of
    { c: _, p: Just p, proxy: _, sessionKey: _, toll: _ } -> do
      masterPassword :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer p) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      { before: masterKey, after: indexReference } <- mapDecodeError $ ExceptT $ splitInHalf <$> (decryptEncryptedRef masterPassword)
      cryptoKey      :: CryptoKey <- ExceptT $ Right <$> KI.importKey raw (toArrayBuffer masterKey) (KI.aes aesCTR) false [encrypt, decrypt, unwrapKey]
      getDecryptedBlob indexReference cryptoKey
    _ -> except $ Left $ InvalidStateError "Missing p"
  
  where 
    decryptEncryptedRef :: CryptoKey -> Aff (Either EX.Error ArrayBuffer)
    decryptEncryptedRef password = decryptArrayBuffer password (toArrayBuffer encryptedRef)
    splitInHalf :: Either EX.Error ArrayBuffer -> Either EX.Error { before :: HexString, after :: HexString }
    splitInHalf either = (fromArrayBuffer >>> splitHexInHalf) <$> either
    mapDecodeError :: ExceptT EX.Error Aff { before :: HexString, after :: HexString } -> ExceptT AppError Aff { before :: HexString, after :: HexString }
    mapDecodeError = withExceptT (\e -> ProtocolError $ DecodeError $ EX.message e)
