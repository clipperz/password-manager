module Functions.Index where

import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except.Trans (ExceptT(..), withExceptT, except)
import Control.Semigroupoid ((>>>))
import Crypto.Subtle.Key.Types (CryptoKey)
import Data.Argonaut.Core (Json)
import Data.Argonaut.Parser (jsonParser)
import Data.ArrayBuffer.Types (ArrayBuffer)
import Data.Codec.Argonaut (decode)
import Data.Function (($))
import Data.HexString (Base(..), fromArrayBuffer, toArrayBuffer, toString)
import Data.List (List(..), (:))
import Data.Show (class Show, show)
import Data.Tuple (Tuple(..))
import DataModel.AppError (AppError(..))
import DataModel.Card (Card(..), CardValues(..), currentCardVersion)
import DataModel.Codec as Codec
import DataModel.Communication.ProtocolError (ProtocolError(..))
import DataModel.Index (CardEntry(..), CardReference(..), Index)
import DataModel.IndexVersions.IndexV1 (Index_V1, indexFromV1)
import DataModel.SRP (HashFunction)
import DataModel.User (IndexReference(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Functions.EncodeDecode (decryptWithAesGCM, encryptJson, exportCryptoKeyToHex, importCryptoKeyAesGCM)

createCardEntry :: Card -> CryptoKey -> HashFunction -> Aff (Tuple ArrayBuffer CardEntry)
createCardEntry card@(Card { content: (CardValues content), archived, timestamp: _ }) key hashf = do
  encryptedCard <- encryptJson Codec.cardCodec key card
  hash <- hashf (encryptedCard : Nil)
  exportedKey <- exportCryptoKeyToHex key
  let cardEntry = CardEntry { title: content.title
                               , cardReference: CardReference { reference: fromArrayBuffer hash, key: exportedKey, cardVersion: currentCardVersion }
                               , archived: archived
                               , tags: content.tags
                               , lastUsed: 0.0
                               }
  pure $ Tuple encryptedCard cardEntry

getIndexContent :: ArrayBuffer -> IndexReference -> ExceptT AppError Aff Index
getIndexContent bytes (IndexReference ref) =
  case ref.indexVersion of 
    "V1"    -> do
      cryptoKey     :: CryptoKey   <- liftAff $ importCryptoKeyAesGCM (toArrayBuffer ref.masterKey)
      decryptedData :: ArrayBuffer <- mapError (ExceptT $ decryptWithAesGCM bytes cryptoKey)
      parsedJson    :: Json        <- mapError (except $ jsonParser $ toString Dec $ fromArrayBuffer decryptedData)
      indexV1       :: Index_V1    <- mapError (except $ decode Codec.indexV1Codec parsedJson)
      pure $ indexFromV1 indexV1
    version -> throwError $ InvalidVersioning version "index"

  where
    mapError :: forall a e. Show e => ExceptT e Aff a -> ExceptT AppError Aff a
    mapError = withExceptT (show >>> CryptoError >>> ProtocolError)
